package cleaner.ui

import cleaner.model.*
import cleaner.config.ConfigService
import cleaner.quarantine.{QuarantineService, StateService}
import cleaner.storage.HistoryService

// связывает пункты меню с поиском, настройками и карантином
class ConsoleMenu(files: ConfigService, initial: AppConfig):
  // текущие настройки, они могут поменяться во время работы
  private var config = initial
  // объекты для истории, состояния карантина и работы с его файлами
  private var history = new HistoryService(config)
  private var quarantine = new QuarantineService(config, new StateService(config), history)
  private var search = new SearchMenu(config, history, quarantine)
  private var entries = new QuarantineMenu(quarantine, files, history, () => search.report())

  // применяем новые настройки и заново создаём связанные с ними объекты
  private def applyConfig(next: AppConfig): Unit =
    // сохраняем историю текущего запуска, чтобы она не потерялась
    val previous = history.session.toList
    history.takeErrors().foreach(e => println(e.message))
    config = next
    history = new HistoryService(config)
    history.session ++= previous
    quarantine = new QuarantineService(config, new StateService(config), history)
    // старые результаты могли уже не подходить под новые настойки
    search = new SearchMenu(config, history, quarantine)
    entries = new QuarantineMenu(quarantine, files, history, () => search.report())

  // выводим все настроенные точки поиска
  private def showRoots(): Unit = config.roots.foreach { r =>
    println(s"${r.id} | ${r.name} | ${r.path} | ${r.profile} | глубина=${r.depth} | активна=${r.enabled} | исключения=${r.exclusions.mkString(",")}")
  }

  // edit=false добавляет точку, edit=true меняет существующую
  private def editRoot(edit: Boolean): Result[Unit] =
    showRoots()
    for
      id <- Input.ask("Идентификатор точки")
      // при изменении точка должна быть, а при добавлении id должен быть свободным
      exists = config.roots.exists(_.id == id)
      _ <- if edit then Result.check(exists, AppError.UnknownRoot(id)) else Result.check(!exists, AppError.DuplicateRoot(id))
      name <- Input.ask("Название")
      path <- Input.ask("Путь").flatMap(files.userPath)
      profile <- Input.ask("Профиль")
      depth <- Input.integer("Глубина")
      enabled <- Input.ask("Активна? true/false").flatMap(s => s.toBooleanOption.toRight(AppError.BadBoolean(s)))
      exclusions <- Input.ask("Исключения через запятую или Enter")
      // собираем точку из ответов пользоватлея
      root = SearchRoot(id, name, path, profile, depth, enabled, Input.ids(exclusions))
      // убираем прежнюю точку с этим id и добавляем новую версию
      next <- files.saveRoots(config.roots.filterNot(_.id == id) :+ root)
    yield
      // сохраняем точки в файл и применяем полученные настройки
      applyConfig(next)
      history.record(if edit then "ROOT_EDIT" else "ROOT_ADD", id)

  // удаляем точку из настроек, саму папку этот метод не удаляет
  private def removeRoot(): Result[Unit] =
    showRoots()
    for
      id <- Input.ask("Идентификатор точки")
      _ <- Result.check(config.roots.exists(_.id == id), AppError.UnknownRoot(id))
      next <- files.saveRoots(config.roots.filterNot(_.id == id))
    yield
      applyConfig(next)
      history.record("ROOT_REMOVE", id)

  // перечитываем config.properties после ручного изменения
  private def reload(): Result[Unit] =
    for
      next <- files.load()
      // пути хранения во время работы менять нельзя, для этого нужен перезапуск
      _ <- Result.check(next.quarantine == config.quarantine && next.stateFile == config.stateFile, AppError.RestartRequired)
    yield
      applyConfig(next)
      history.record("RELOAD", "Настройки загружены; результаты сброшены")

  // вызывается из меню когда пользователь выбирает просмотр истории
  private def showHistory(): Result[Unit] = history.rows().map { rows =>
    if rows.isEmpty then println("История пока пуста.")
    // проверяем количество полей, неправильную строку пропуксаем
    // для вывода заменяем табуляцию на понятный разделитель
    rows.foreach(row => println(if row.size == 5 then row.mkString(" | ") else "Повреждённая строка истории."))
  }

  // ## сюда уже пришла команда, цифру из консоли второй раз разбирать не надо
  private def execute(command: Command): Result[Unit] = command match
    case Command.ShowRoots => Right(showRoots())
    case Command.AddRoot => editRoot(false)
    case Command.EditRoot => editRoot(true)
    case Command.RemoveRoot => removeRoot()
    case Command.Profiles => Right(search.profiles())
    case Command.Scan => search.scan()
    case Command.Results => search.results()
    case Command.Details => search.details()
    case Command.Select => search.select()
    case Command.Move => search.move()
    case Command.ShowEntries => entries.show()
    case Command.Restore => entries.restore()
    // ## для extend идём в меню карантина, там спросят номер записи и на сколько дней продлить
    case Command.Extend => entries.extend()
    case Command.Purge => entries.purgeSelected()
    case Command.Expired => entries.expired()
    case Command.History => showHistory()
    case Command.Report => search.report()
    case Command.Reload => reload()

  // управляет меню: показывает пункты и читает номер от пользователя
  // ## ноль вернёт none, из подменю попадём назад, из главного меню выйдем
  private def choose[A](title: String, items: List[A], label: A => String, back: String): Result[Option[A]] =
    println("\n" + title)
    items.zipWithIndex.foreach((item, i) => println(s"${i + 1}. ${label(item)}"))
    println("0. " + back)
    Input.integer("Введите номер").flatMap { n =>
      // 0 означает назад, а в главном меню — выход
      if n == 0 then Right(None)
      // ## пункты на экране с единицы, а в списке с нуля, поэтому берём n минус один
      else items.lift(n - 1).map(Some(_)).toRight(AppError.UnknownCommand(n.toString))
    }.left.map {
      case AppError.EndOfInput => AppError.EndOfInput
      case error => AppError.InvalidMenuChoice(error.message)
    }

  // этот метод запускается из Main
  def run(): Unit =
    // ## пока раздел не выбран, показываем главное меню
    var section = Option.empty[Section]
    var running = true
    def handle(result: Result[Unit]): Unit =
      result match
        // если ввод закончился, просто завершаем работу
        // Если ввод закрыт, завершаем работу, а не повторяем чтение бесконечно
        case Left(AppError.EndOfInput) => running = false
        // при неправильном вводе показывает сообщение и снова ждёт выбор
        case Left(AppError.InvalidMenuChoice(reason)) =>
          println(reason)
          history.record("UNKNOWN_COMMAND", "Неверный номер пункта")
        // выводим причину ошибки и записываем её тип в журнал
        case Left(error) =>
          println("Операция не выполнена: " + error.message)
          history.record("ERROR", history.errorDetails(error), "ERROR")
        case Right(_) => ()
      // если запись не удалась, показываем тип ошибки
      history.takeErrors().foreach(error => println(error.message))

    history.record("START", "Программа запущена")
    // при старте показываем карантин
    handle(entries.show().flatMap { _ =>
      // если настройка включена и есть просроченное, спрашиваем о нём
      if !config.askExpired then Right(())
      else quarantine.expired.flatMap(items => if items.nonEmpty then entries.expired() else Right(()))
    })
    while running do
      section match
        case None =>
          handle(choose("Менеджер временных файлов", Section.values.toList, _.title, "Выход").map { choice =>
            section = choice
            running = choice.nonEmpty
          })
        case Some(current) =>
          val commands = Command.values.toList.filter(_.section == current)
          handle(choose(current.title, commands, _.title, "Назад").flatMap {
            case None =>
              section = None
              Right(())
            case Some(command) => execute(command)
          })
    // завершение записываем и при обычном выходе, и при ошибке
    history.record("EXIT", "Программа завершена")
    history.takeErrors().foreach(e => println(e.message))
