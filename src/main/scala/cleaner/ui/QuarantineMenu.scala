package cleaner.ui

import cleaner.model.*
import cleaner.config.ConfigService
import cleaner.quarantine.QuarantineService
import cleaner.storage.{FileService, HistoryService}
import java.nio.file.Path
import java.time.Instant

class QuarantineMenu(service: QuarantineService, config: ConfigService, history: HistoryService,
                     saveReport: () => Result[Unit]):
  // показываем записи карантина и предупреждения о незавершённых операциях
  def show(): Result[Unit] = service.list().map { list =>
    if list.entries.isEmpty then println("Подтверждённых записей нет.")
    list.entries.foreach { e =>
      println(s"${e.id} | ${e.name} | ${e.size} байт | до ${Instant.ofEpochMilli(e.expiresAt)} | ${e.status.key} | ${e.original}")
      if list.missing.contains(e.id) then println("  Данные записи отсутствуют.")
      if e.status != EntryStatus.Active then println("  Незавершённая операция. Нужна ручная проверка.")
    }
    list.unknownFiles.foreach(p => println(s"Неподтверждённый объект: $p"))
  }

  private def destination(e: QuarantineEntry): Result[Option[(Path, Boolean)]] =
    // если исходный путь свободен, можно оставить его или указать другой
    if !FileService.exists(e.original) then
      Input.optional("Путь для восстановления или Enter для исходного").flatMap {
        // сначала предполагаем восстановление на прежнее место
        case None => Right(Some((e.original, false)))
        case Some(s) => config.userPath(s).map(p => Some((p, false)))
      }
    else
      // если место занято, пользователь решает как поступить
      Input.choice("Путь занят: name, folder, replace, cancel", "name" -> Conflict.Name,
        "folder" -> Conflict.Folder, "replace" -> Conflict.Replace, "cancel" -> Conflict.Cancel).flatMap {
        // ## отмена тут нормальный ответ, возвращаем none и файл останется где был
        case Conflict.Cancel => Right(None)
        // замена требует отдельного подтверждения
        case Conflict.Replace => Input.confirm("Сохранить прежний объект в карантине и заменить?")
          .map(ok => Option.when(ok)((e.original, true)))
        // берём другую папку, имя объекта оставляем прежним
        case Conflict.Folder => Input.ask("Другая папка").flatMap(config.userPath).map(p => Some((p.resolve(e.name), false)))
        case Conflict.Name =>
          for
            name <- Input.ask("Новое имя без пути")
            parsed <- Result.io(Path.of(name)).left.map(_ => AppError.BadPath(name))
            // здесь нужно только имя, переходы в другие папки не разрешаем
            _ <- Result.check(name.nonEmpty && name != "." && name != ".." && Option(parsed.getRoot).isEmpty &&
              parsed.getNameCount == 1 && !name.contains("/") && !name.contains("\\"), AppError.BadPath(name))
          yield Some((e.original.resolveSibling(parsed), false))
      }

  // возвращаем объект из карантина по исходному или другому пути
  def restore(): Result[Unit] =
    for
      _ <- show()
      id <- Input.ask("Полный номер записи")
      e <- service.find(id)
      target <- destination(e)
      _ <- target match
        case None => Right(())
        case Some((path, replace)) =>
          // истечение срока не запрещает восстановление пока данные не удалены
          if e.expiresAt <= System.currentTimeMillis() then println("Срок истёк. Файл ещё можно восстановить.")
          service.restore(id, path, replace).flatMap { result =>
            result.backup.foreach(b => println("Прежний объект сохранён в карантине: " + b.id))
            println("Восстановлено: " + result.target)
            saveReport()
          }
    yield ()

  // добавляем дни к сроку хранения одной записи карантина
  def extend(): Result[Unit] =
    for
      id <- Input.ask("Полный номер записи")
      days <- Input.integer("На сколько дней")
      // ## если id не найден, сервис вернёт left и строка с новой датой не напечатается
      e <- service.extend(id, days)
    // ## сюда дойдём только если сервис сохранил запись, печатаем её новую дату
    yield println("Новый срок: " + Instant.ofEpochMilli(e.expiresAt))

  // окончательное удаление перечисленных записей, сначала обязательно спрашиваем
  private def purge(items: List[QuarantineEntry]): Result[Unit] =
    if items.isEmpty then Right(println("Нет подходящих записей."))
    else
      items.foreach(e => println(s"${e.id} | ${e.name} | ${e.size} байт"))
      Input.confirm("Удалить эти объекты окончательно?").flatMap { yes =>
        if !yes then Right(())
        else
          val results = service.purge(items)
          // ## в счётчик удалённых попадают только записи с right
          val deleted = results.collect { case (e, Right(_)) => e }
          results.foreach { case (e, r) => r.left.foreach(error => println(s"${e.id}: ${error.message}")) }
          println(s"Удалено объектов: ${deleted.size}; освобождено байт: ${deleted.map(_.size).sum}; ошибок: ${items.size - deleted.size}")
          saveReport()
      }

  // получаем выбранные записи карантина по их длинным номерам
  def purgeSelected(): Result[Unit] =
    Input.ask("Полные номера через запятую").flatMap(s => Result.collect(Input.ids(s))(service.find)).flatMap(purge)

  // показываем записи с истёкшим сроком и спрашиваем что с ними делать
  def expired(): Result[Unit] = service.expired.flatMap { items =>
    history.record("EXPIRED_CHECK", "count=" + items.size)
    if items.isEmpty then Right(println("Просроченных записей нет."))
    else
      // считаем сколько полных дней прошло с самого раннего перемещения
      val oldest = (System.currentTimeMillis() - items.map(_.movedAt).min) / 86400000L
      println(s"Просрочено: ${items.size}; объём: ${items.map(_.size).sum} байт; старейшему объекту $oldest дней")
      Input.choice("list — список, all — удалить все, ids — выбрать, extend — продлить, Enter — оставить",
        "list" -> ExpiredAction.ListEntries, "all" -> ExpiredAction.DeleteAll, "ids" -> ExpiredAction.DeleteIds,
        "extend" -> ExpiredAction.Extend, "" -> ExpiredAction.Keep).flatMap {
        case ExpiredAction.Keep => Right(())
        case ExpiredAction.ListEntries => Right(items.foreach(e => println(s"${e.id} | ${e.original} | до ${Instant.ofEpochMilli(e.expiresAt)}")))
        case ExpiredAction.Extend => extend()
        case ExpiredAction.DeleteAll => purge(items)
        case ExpiredAction.DeleteIds => Input.ask("Полные номера через запятую").flatMap { s =>
          // проверяем что все введённые номера есть среди просроченных
          Result.collect(Input.ids(s))(id => items.find(_.id == id).toRight(AppError.UnknownEntry(id))).flatMap(purge)
        }
      }
  }
