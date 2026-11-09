package cleaner.config

import cleaner.model.*
import cleaner.storage.FileService
import java.nio.file.{FileSystems, Files, Paths}
import java.util.regex.Pattern

object ConfigCheck:
  // ## если проверка не прошла, передаём причину наверх через left
  private def check(ok: Boolean, why: String) = Result.check(ok, AppError.BadConfig(why))

  // вызывается при проверке настроек, пробуем создать проверку по маске
  // если маска написана неправильно, будет ошибка
  private def mask(value: String): Result[Unit] =
    Result.io(FileSystems.getDefault.getPathMatcher("glob:" + value))
      .map(_ => ()).left.map(_ => AppError.BadConfig(s"Неверная маска: $value"))

  // проверяем что настройки подходят для работы программы
  def validate(c: AppConfig): Result[Unit] =
    // собираем существующие названия и id для следующих проверок
    val rules = c.rules.map(_.id)
    val categories = c.rules.map(_.category)
    val exclusions = c.exclusions.map(_.id)
    // настройки, история и остальные служебные файлы должны иметь разные пути
    val files = List(c.file, c.stateFile, c.logFile, c.historyFile, c.reportFile)
    val home = Paths.get(System.getProperty("user.home")).toAbsolutePath.normalize()

    // проверяем каждую точку: доступ, профиль, исключения и расположение карантина
    def checkRoot(r: SearchRoot): Result[Unit] =
      for
        _ <- check(r.depth >= 0, s"Отрицательная глубина: ${r.id}")
        _ <- c.profile(r.profile)
        _ <- FileService.checkNoLinks(r.path)
        _ <- check(Files.isDirectory(r.path) && Files.isReadable(r.path), s"Точка недоступна: ${r.path}")
        // пробуем открыть папку, сразу закрываем, здесь её файлы ещё не ищем
        _ <- Result.io(Files.newDirectoryStream(r.path).close())
        _ <- check(!c.technicalPaths.exists(r.path.startsWith(_)), "Точка в служебном каталоге")
        _ <- check(!r.enabled || !c.quarantine.startsWith(r.path), "Карантин внутри точки поиска")
        _ <- check(r.exclusions.forall(exclusions.contains), "Неизвестное исключение точки")
      yield ()

    // у правил проверяем тип объекта, размеры, сроки и правильность масок
    def checkRule(r: Rule): Result[Unit] =
      for
        _ <- check(Set("file","directory","any").contains(r.targetType), s"Неверный тип: ${r.id}")
        _ <- check(r.minimumAgeHours >= 0 && r.minimumSize >= 0 && r.maximumSize.forall(_ >= r.minimumSize),
          s"Неверные ограничения: ${r.id}")
        _ <- Result.collect(r.patterns ++ r.parentContains ++ r.contains ++ r.relativePattern.toList)(mask)
        // пробуем собрать регулярное выражение, если оно кривое будет ошибка
        _ <- Result.collect(r.regex.toList)(v => Result.io(Pattern.compile(v)).left.map(_ => AppError.BadConfig(v)))
      yield ()

    // профиль не должен ссылаться на несуществующие правила или категории
    def checkProfile(p: Profile): Result[Unit] =
      for
        _ <- check((p.rules ++ p.excludedRules).forall(rules.contains), s"Неизвестное правило: ${p.id}")
        _ <- check(p.categories.forall(categories.contains), s"Неизвестная категория: ${p.id}")
        _ <- check(p.maximumSize.forall(_ >= 0), s"Отрицательный размер: ${p.id}")
      yield ()

    // исключение тоже проверяем, чтобы не было непонятного типа или пустого значения
    def checkExclusion(e: Exclusion): Result[Unit] =
      val valid = e.kind match
        case "category" => categories.contains(e.value)
        case "rule" => rules.contains(e.value)
        case "youngerHours" => e.value.toLongOption.exists(_ >= 0)
        case "path" | "tree" | "name" | "mask" | "extension" => true
        case _ => false
      for
        _ <- check(valid, s"Неверное исключение: ${e.id}")
        _ <- Result.collect(e.scope.toList)(mask)
        _ <- if e.kind == "mask" then mask(e.value) else Right(())
      yield ()

    for
      // у точек, правил, профилей и исключений проверяем их идентификаторы
      _ <- Result.collect(List(c.roots.map(_.id), rules, c.profiles.map(_.id), exclusions)) { ids =>
        // distinct убирает повторы, если размер уменьшился значит были одинаковые id
        check(ids == ids.distinct && ids.forall(_.matches("[A-Za-z0-9_-]+")), "Повтор или неверный ID")
      }
      // отрицательная глубина и отрицательный срок нам не подходят
      _ <- check(c.maximumDepth >= 0 && c.defaultRetentionDays >= 0, "Отрицательная глубина или срок")
      // этот вызов выдаст ошибку, если профиля по умолчанию нет
      _ <- c.profile(c.defaultProfile)
      // нельзя сделать весь диск или домашнюю папку карантином
      _ <- check(c.quarantine != c.quarantine.getRoot && c.quarantine != home, "Недопустимый каталог карантина")
      _ <- FileService.checkNoLinks(c.quarantine)
      // карантин должен существовать и быть доступным для записи
      _ <- check(Files.isDirectory(c.quarantine) && Files.isWritable(c.quarantine), "Каталог карантина недоступен")
      _ <- check(files.distinct == files && !files.exists(_.startsWith(c.quarantine)), "Проверьте служебные пути")
      _ <- Result.collect(files)(FileService.checkNoLinks)
      _ <- check(c.globalExclusions.forall(exclusions.contains), "Неизвестное общее исключение")
      _ <- Result.collect(c.roots)(checkRoot)
      _ <- Result.collect(c.rules)(checkRule)
      _ <- Result.collect(c.profiles)(checkProfile)
      _ <- Result.collect(c.exclusions)(checkExclusion)
    yield ()
