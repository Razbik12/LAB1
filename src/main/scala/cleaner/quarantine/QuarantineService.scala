package cleaner.quarantine

import cleaner.model.*
import cleaner.search.RuleMatcher
import cleaner.storage.{FileService, HistoryService}
import java.nio.file.{Files, Path}
import java.util.UUID

case class QuarantineList(entries: List[QuarantineEntry], missing: List[String], unknownFiles: List[Path])
case class Restored(target: Path, backup: Option[QuarantineEntry])

class QuarantineService(config: AppConfig, state: StateService, history: HistoryService):
  private val matcher = new RuleMatcher(config)
  // объект хранится в карантине под уникальным номером записи
  private def stored(e: QuarantineEntry): Path = config.quarantine.resolve(e.id)

  def volume: Result[Long] = state.entries.map(_.map(_.size).sum)
  private def volumeDetails: String = volume.fold(history.errorDetails, _.toString)
  def expired: Result[List[QuarantineEntry]] =
    // берём записи с завершённым помещением в карантин
    state.entries.map(_.filter(e => e.status == EntryStatus.Active && e.expiresAt <= System.currentTimeMillis()))

  def find(id: String): Result[QuarantineEntry] =
    // ## если нет записи с таким id, вернём ошибку UnknownEntry с этим номером
    state.entries.flatMap(_.find(_.id == id).toRight(AppError.UnknownEntry(id)))

  def list(): Result[QuarantineList] =
    for
      entries <- state.entries
      paths <- FileService.children(config.quarantine)
    yield QuarantineList(entries, entries.filterNot(e => FileService.exists(stored(e))).map(_.id),
      paths.filterNot(p => entries.exists(_.id == p.getFileName.toString)))

  private def space(path: Path, bytes: Long): Result[Unit] =
    Result.io(Files.getFileStore(path).getUsableSpace).flatMap(free =>
      Result.check(free >= bytes, AppError.NoSpace(path.toString)))

  // добавляем указанное количество дней к начальному времени
  private def deadline(start: Long, days: Int): Result[Long] =
    Result.io(Math.addExact(start, Math.multiplyExact(days.toLong, 86400000L))).left.map(_ => AppError.InvalidDays)

  // помещаем один объект в карантин и создаём запись для восстановления
  private def put(path: Path, root: Path, info: FileInfo, days: Int, rule: String, category: String): Result[QuarantineEntry] =
    val now = System.currentTimeMillis()
    for
      _ <- Result.check(days >= 0, AppError.InvalidDays)
      _ <- Result.check(Files.isWritable(path.getParent), AppError.NoAccess(path.toString))
      _ <- space(config.quarantine, info.size)
      until <- deadline(now, days)
      entry = QuarantineEntry(UUID.randomUUID().toString, path, root, path.getFileName.toString,
        info.isDirectory, info.size, now, days, until, rule, category, info.fingerprint, EntryStatus.Pending, None)
      // ## сначало отмечаем что перенос начался, потом трогаем сам файл
      _ <- state.update(entry)
      _ <- FileService.move(path, stored(entry))
      moved <- FileService.info(stored(entry))
      _ <- Result.check(moved == info && !FileService.exists(path), AppError.FileChanged(path.toString))
      active = entry.copy(status = EntryStatus.Active)
      _ <- state.update(active)
      // ## прежде чем сказать меню что всё готово, сверяем запись с файлом состояния
      _ <- state.verifySaved(active)
    yield active

  // повторно проверяем выбранные объекты и помещаем каждый в карантин
  def quarantine(items: List[Candidate], days: Option[Int], foundCount: Int): List[(Candidate, Result[QuarantineEntry])] =
    // один номер связывает все события этой очистки
    val operation = UUID.randomUUID().toString
    val details = s"roots=${items.map(_.root.id).distinct.mkString(",")}; profiles=${items.map(_.profile).distinct.mkString(",")}; " +
      s"found=$foundCount; selectedCount=${items.size}; selected=${items.map(c => s"${c.id}:${history.path(c)}").mkString(",")}"
    history.record("QUARANTINE_START", details, operationId = operation)
    val results = items.map { c =>
      history.record("RECHECK", history.path(c), operationId = operation)
      val result = for
        info <- FileService.info(c.path)
        // сведения о файле могли измениться пока пользователь выбирал объекты
        _ <- Result.check(info == c.info, AppError.FileChanged(c.path.toString))
        fits <- matcher.matches(c.rule, c.path, c.root, info)
        _ <- Result.check(fits, AppError.RuleChanged(c.path.toString))
        _ <- matcher.checkSafe(c.path, c.root, c.rule)
        // срок пользователя важнее срока сохранённого у кандидата
        // put перемещает один объект и возвращает запись карантина
        entry <- put(c.path, c.root.path, info, days.getOrElse(c.retentionDays), c.rule.id, c.rule.category)
      yield entry
      result match
        case Right(e) => history.record("QUARANTINE_OK", s"${history.path(c)}; id=${e.id}; bytes=${e.size}; days=${e.retentionDays}", operationId = operation)
        case Left(e) => history.record("QUARANTINE_ERROR", s"${history.path(c)}; ${history.errorDetails(e)}", "ERROR", operation)
      // ## возвращаем сам объект и его результат, так меню знает что именно не перенеслось
      (c, result)
    }
    val successes = results.flatMap(_._2.toOption)
    val errors = results.size - successes.size
    val status = if errors == 0 then "OK" else if successes.isEmpty then "ERROR" else "PARTIAL"
    // сохраняем итог всей операции
    history.record("QUARANTINE_END", s"$details; moved=${successes.size}; sourceBytes=${successes.map(_.size).sum}; quarantineBytes=$volumeDetails; errors=$errors", status, operation)
    results

  // проверяем состояние записи и сохранённые метаданные объекта
  private def verify(e: QuarantineEntry): Result[Unit] =
    for
      _ <- Result.check(e.status == EntryStatus.Active, AppError.NotActive(e.id))
      info <- FileService.info(stored(e))
      _ <- Result.check(info.size == e.size && info.isDirectory == e.isDirectory && info.fingerprint == e.fingerprint,
        AppError.FileChanged(stored(e).toString))
    yield ()

  private def backup(target: Path, replace: Boolean): Result[Option[QuarantineEntry]] =
    if !FileService.exists(target) then Right(None)
    // ## если замена не подтверждена, занятый путь оставляем как есть
    else if !replace then Left(AppError.Occupied(target.toString))
    else
      for
        tree <- FileService.tree(target)
        _ <- Result.check(!tree.exists(matcher.protectedPath), AppError.Protected(target.toString))
        info <- FileService.info(target)
        // Заменяемый объект тоже сохраняем в карантине, а не удаляем.
        // прежний объект сохраняем в карантине перед заменой
        entry <- put(target, target.getParent, info, config.defaultRetentionDays, "manual-replacement", "replacement-backup")
      yield
        history.record("REPLACEMENT_BACKUP", "id=" + entry.id)
        Some(entry)

  def restore(id: String, target: Path, replace: Boolean): Result[Restored] =
    for
      e <- find(id)
      _ <- verify(e)
      // проверяем путь назначения и доступ к родительской папке
      _ <- FileService.checkNoLinks(target)
      _ <- Result.check(!matcher.protectedPath(target), AppError.Protected(target.toString))
      parent <- Option(target.getParent).toRight(AppError.BadPath(target.toString))
      _ <- Result.check(Files.isDirectory(parent) && Files.isWritable(parent), AppError.NoAccess(parent.toString))
      _ <- space(parent, e.size)
      saved <- backup(target, replace)
      // сохраняем начало восстановления на случай прерывания
      _ <- state.update(e.copy(status = EntryStatus.Restoring, restoreTarget = Some(target)))
      _ <- FileService.move(stored(e), target)
      // проверяем что объект появился на месте восстановления
      // и больше не находится в карантине
      info <- FileService.info(target)
      _ <- Result.check(!FileService.exists(stored(e)) && info.size == e.size && info.fingerprint == e.fingerprint,
        AppError.FileChanged(target.toString))
      // запись больше не нужна в активном состоянии карантина
      _ <- state.remove(id)
    yield
      history.record("RESTORE", history.restoredDetails(e, target))
      Restored(target, saved)

  def extend(id: String, days: Int): Result[QuarantineEntry] =
    for
      // ## ноль или минус дней не подходят, при left остальные шаги не выполнятся
      _ <- Result.check(days > 0, AppError.InvalidDays)
      e <- find(id)
      _ <- Result.check(e.status == EntryStatus.Active, AppError.NotActive(id))
      // ## если срок уже вышел, новые дни считаем от сегодня
      until <- deadline(math.max(System.currentTimeMillis(), e.expiresAt), days)
      total <- Result.io(Math.addExact(e.retentionDays, days)).left.map(_ => AppError.InvalidDays)
      updated = e.copy(retentionDays = total, expiresAt = until)
      // ## сохраняем новый срок и только потом отдаём обновлённую запись меню
      _ <- state.update(updated)
    yield
      history.record("EXTEND", s"id=$id; days=$days; until=$until")
      updated

  // окончательно удаляем выбранные просроченные объекты
  // меню вызывает этот метод только после подтверждения
  def purge(items: List[QuarantineEntry]): List[(QuarantineEntry, Result[Unit])] =
    val results = items.map { item =>
      val result = for
        e <- find(item.id)
        _ <- Result.check(e.expiresAt <= System.currentTimeMillis(), AppError.NotExpired(e.id))
        _ <- verify(e)
        // ## отмечаем начало удаления заранее, чтобы после сбоя было видно что случилось
        _ <- state.update(e.copy(status = EntryStatus.Deleting))
        _ <- FileService.deleteTree(stored(e))
        _ <- Result.check(!FileService.exists(stored(e)), AppError.FileChanged(stored(e).toString))
        _ <- state.remove(e.id)
      yield history.record("PURGE", s"id=${e.id}; bytes=${e.size}")
      result.left.foreach(e => history.record("PURGE_ERROR", s"id=${item.id}; ${history.errorDetails(e)}", "ERROR"))
      (item, result)
    }
    val deleted = results.collect { case (e, Right(_)) => e }
    val errors = items.size - deleted.size
    history.record("PURGE_END", s"deleted=${deleted.size}; bytes=${deleted.map(_.size).sum}; errors=$errors; quarantineBytes=$volumeDetails",
      if errors == 0 then "OK" else "PARTIAL")
    results
