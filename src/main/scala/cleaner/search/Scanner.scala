package cleaner.search

import cleaner.model.*
import cleaner.storage.{FileService, HistoryService}
import java.nio.file.{Files, LinkOption, Path}
import scala.collection.mutable.ListBuffer

// ## сканер отдаёт событие, а строку на экран уже печатает меню
enum ScanEvent:
  case Started(root: Path)
  case Progress(checked: Long, found: Int, bytes: Long, folder: Path)

class Scanner(config: AppConfig, history: HistoryService):
  private val matcher = new RuleMatcher(config)

  private def candidate(path: Path, root: SearchRoot, profile: Profile): Result[Option[Candidate]] =
    // выбираем подходящие правила и основное из них
    val rules = config.rulesFor(profile).filter(matcher.possible(_, path, root))
    // ## папку проверили, но кандидата нет, поэтому right с none
    if rules.isEmpty || matcher.protectedPath(path) then Right(None)
    else
      for
        info <- FileService.info(path)
        matches <- Result.collect(rules) { rule =>
          for
            fits <- matcher.matches(rule, path, root, info)
            skip <- matcher.excluded(path, root, Some(rule))
          yield Option.when(fits && !skip)(rule)
        }
        ordered = matches.flatten.sortBy(r => (-r.priority.toLong, -r.specificity, config.rules.indexOf(r)))
        item <- ordered.headOption.filter(_ => profile.maximumSize.forall(info.size <= _)) match
          case None => Right(None)
          // проверяем безопасность определяем срок и создаём Candidate
          case Some(rule) => matcher.checkSafe(path, root, rule).map { _ =>
            // ## срок берём из профиля, потом из правила, а если оба пустые — из общих настроек
            val days = profile.retentionDays.orElse(rule.retentionDays).getOrElse(config.defaultRetentionDays)
            Some(Candidate(0, path, root, profile.id, root.path.relativize(path).toString, info,
              rule, ordered.drop(1).map(_.id), days, matcher.reason(rule),
              List("Проверьте объект перед перемещением в карантин.")))
          }
      yield item

  // обходит выбранные папки и собирает кандидатов на очистку
  // объекты проверяет через RuleMatcher, найденное сохраняет как Candidate
  def scan(roots: List[SearchRoot], overrideProfile: Option[String], notify: ScanEvent => Unit): Result[ScanResult] =
    val found = ListBuffer.empty[Candidate]
    val skips = ListBuffer.empty[Skipped]
    val warnings = ListBuffer.empty[String]
    var checked = 0L
    val profiles = roots.map(r => overrideProfile.getOrElse(r.profile))
    history.record("SCAN_START", s"roots=${roots.map(_.id).mkString(",")}; profiles=${profiles.mkString(",")}")
    for a <- roots; b <- roots if a.id < b.id && (a.path.startsWith(b.path) || b.path.startsWith(a.path)) do
      warnings += s"Пересекаются точки ${a.id} и ${b.id}. Повторы будут убраны."

    // ошибка одного объекта не должна останавливать весь поиск
    // сохраняем причину пропуска и записываем ошибку в историю
    def failed(path: Path, root: SearchRoot, error: AppError): Unit =
      skips += Skipped(path, SkipReason.Failed(error))
      error match
        case AppError.Excluded(_) | AppError.Protected(_) => ()
        case _ => history.record("SCAN_ERROR", s"${root.id}; ${history.errorDetails(error)}", "ERROR")

    def visit(path: Path, root: SearchRoot, profile: Profile, depth: Int): Unit =
      //Проверяем, не прервана ли операция
      if Thread.currentThread().isInterrupted then return
      checked += 1
      //каждые 100 объектов программа показывает ход поиска:
      // сколько проверено, сколько найдено и какой объём набран.
      if checked % 100 == 0 then notify(ScanEvent.Progress(checked, found.size, found.map(_.info.size).sum, path.getParent))
      val allowed = for
        //проверяем путь на ссылки
        _ <- FileService.checkNoLinks(path)
        //пропускаем служебные объекты
        _ <- Result.check(!config.technicalPaths.exists(path.startsWith(_)), AppError.Protected(path.toString))
        //проверяем исключения
        skip <- matcher.excluded(path, root)
        _ <- Result.check(!skip, AppError.Excluded(path.toString))
      yield ()
      allowed match
        case Left(error) => failed(path, root, error)
        case Right(_) =>
          candidate(path, root, profile) match
            // ## файл подошёл, добавляем его в список и продолжаем поиск
            case Right(Some(item)) =>
              // добавляем его в результаты, само перемещение будет позже через меню
              found += item
              history.record("CANDIDATE", history.path(item) + "; rule=" + item.rule.id)
            case other =>
              other.left.foreach(error => failed(path, root, error))
              // если папка сама не выбрана кандидатом, проверяем её содержимое
              if Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) then
                if depth >= math.min(root.depth, config.maximumDepth) then skips += Skipped(path, SkipReason.DepthLimit)
                // получаем детей папки и каждый передаём в этот же метод visit
                else FileService.children(path).fold(error => failed(path, root, error), _.foreach(visit(_, root, profile, depth + 1)))

    Result.collect(roots)(r => config.profile(overrideProfile.getOrElse(r.profile)).map(p => (r, p))).map { pairs =>
      for (root, profile) <- pairs if !Thread.currentThread().isInterrupted do
        notify(ScanEvent.Started(root.path))
        if math.min(root.depth, config.maximumDepth) == 0 then skips += Skipped(root.path, SkipReason.DepthLimit)
        else FileService.checkNoLinks(root.path).flatMap(_ => FileService.children(root.path))
          .fold(error => failed(root.path, root, error), _.foreach(visit(_, root, profile, 1)))
      if Thread.currentThread().isInterrupted then warnings += "Поиск прерван. Показан неполный результат."
      // список кандидатов без повторов и вложенных объектов выбранных папок
      val unique = found.toList.sortBy(_.path.getNameCount).foldLeft(List.empty[Candidate]) { (kept, c) =>
        // ## если папка уже в списке, её вложенные файлы второй раз не добавляем
        if kept.exists(k => c.path.startsWith(k.path)) then kept else c :: kept
      }.reverse
      // назначаем номера 1, 2, 3 и так далеедальш
      val items = unique.zipWithIndex.map { case (c, i) => c.copy(id = i + 1) }
      history.record("SCAN_END", s"checked=$checked; found=${items.size}; errorsOrSkipped=${skips.size}")
      ScanResult(items, checked, roots.map(_.id), profiles, skips.toList, warnings.toList)
    }
