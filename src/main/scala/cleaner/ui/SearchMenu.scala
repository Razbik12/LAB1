package cleaner.ui

import cleaner.model.*
import cleaner.config.ConfigCheck
import cleaner.search.{Scanner, ScanEvent}
import cleaner.quarantine.QuarantineService
import cleaner.storage.{HistoryService, ReportService}

class SearchMenu(config: AppConfig, history: HistoryService, quarantine: QuarantineService):
  // показывает список найденного и подробности об объектах
  private val view = new ResultView()
  // пока поиск не запускали, результат пустой
  private var lastScan: Option[ScanResult] = None
  // ## до первого поиска список пустой, после поиска берём кандидатов из последнего результата
  private def candidates: List[Candidate] = lastScan.toList.flatMap(_.candidates)
  // сюда попадают объекты, которые пользователь выбрал для карантина
  private var selected = List.empty[Candidate]

  // сохраняем отчёт о поиске и операциях текущего запуска
  def report(): Result[Unit] =
    new ReportService(config, history).save(lastScan, quarantine).map(path => println("Отчёт: " + path))

  // показываем профили и какие правила или категории в них входят
  def profiles(): Unit = config.profiles.foreach { p =>
    println(s"${p.id} | категории=${p.categories.mkString(",")} | правила=${p.rules.mkString(",")} | исключены=${p.excludedRules.mkString(",")}")
  }

  // ## сканер сообщает сколько проверил, а печатает это уже меню
  private def progress(event: ScanEvent): Unit = event match
    case ScanEvent.Started(path) => println("Сканирование: " + path)
    case ScanEvent.Progress(count, found, bytes, path) =>
      println(s"Проверено: $count; кандидатов: $found; байт: $bytes; каталог: $path")

  // спрашиваем где искать и с каким профилем, потом запускаем поиск
  def scan(): Result[Unit] =
    config.roots.foreach(r => println(s"${r.id} | ${r.path} | профиль=${r.profile}"))
    for
      answer <- Input.ask("Точки через запятую или all для всех активных")
      // all выбирает активные точки, иначе ищем точки по введённым id
      roots <- if answer == "all" then Right(config.roots.filter(_.enabled))
               else Result.collect(Input.ids(answer))(id => config.roots.find(_.id == id).toRight(AppError.UnknownRoot(id)))
      _ <- Result.check(roots.nonEmpty, AppError.NoRoots)
      // пустой ответ оставляет профиль, который указан у каждой точки
      profile <- Input.optional("Профиль или Enter для профиля точки")
      // папки могли исчезнуть после запуска, поэтому проверяем настройки ещё раз
      _ <- ConfigCheck.validate(config)
      found <- new Scanner(config, history).scan(roots, profile, progress)
      _ = {
        lastScan = Some(found)
        // выбор из прошлого поиска больше не нужен
        selected = Nil
        // сразу показываем найденное по категориям, большие объекты идут первыми
        view.show(found.candidates, Group.Category, Sort.Size, None)
        found.warnings.foreach(println)
        println("Пропусков/ошибок: " + found.skipped.size)
      }
      _ <- report()
    yield ()

  // пользователь выбирает группировку, сортировку и фильтр результатов
  def results(): Result[Unit] =
    for
      group <- Input.choice("Группировка: root, category, rule", "root" -> Group.Root, "category" -> Group.Category, "rule" -> Group.Rule)
      sort <- Input.choice("Сортировка: size, age, category, path", "size" -> Sort.Size, "age" -> Sort.Age,
        "category" -> Sort.Category, "path" -> Sort.Path)
      filter <- Input.optional("Фильтр: путь, категория или Enter")
    yield view.show(candidates, group, sort, filter)

  // находим кандидата по номеру и показываем все сведения о нём
  def details(): Result[Unit] =
    for
      id <- Input.integer("Номер объекта")
      item <- candidates.find(_.id == id).toRight(AppError.UnknownCandidate(id))
    yield view.details(item)

  // выбераем какие найденные объекты потом переносить в карантин
  def select(): Result[Unit] =
    for
      choice <- Input.selection()
      items <- choice.select(candidates)
    yield
      selected = items
      println("Выбрано: " + selected.size)

  // показываем план и переносим выбранное только после подтверждения
  def move(): Result[Unit] =
    if selected.isEmpty then return Left(AppError.EmptySelection)
    view.show(selected, Group.Category, Sort.Path, None)
    for
      value <- Input.optional("Срок в днях или Enter для срока правила")
      // ## нажали enter — оставим none, ввели число — проверим и положим в some
      days <- value match
        case None => Right(None)
        case Some(s) => Input.number(s).flatMap(n => Result.check(n >= 0, AppError.InvalidDays).map(_ => Some(n)))
      // показываем какой срок будет у каждого выбранного объекта
      _ = selected.foreach(c => println(s"${c.relative} — ${days.getOrElse(c.retentionDays)} дней"))
      ok <- Input.confirm("Поместить эти объекты в карантин?")
      _ <- if !ok then Right(()) else
        val moved = quarantine.quarantine(selected, days, candidates.size)
        // ## для каждого файла показываем свой итог, один мог переехать, а другой остаться на месте
        moved.foreach {
          case (c, Right(e)) => println(s"Перемещён: ${c.relative}; номер: ${e.id}")
          case (c, Left(error)) => println(s"Пропущен ${c.relative}: ${error.message}")
        }
        selected = Nil
        report()
    yield ()
