package cleaner.config

import cleaner.model.*
import cleaner.storage.*
import java.nio.file.{Files, Path, Paths}
import java.util.Properties

// file хранит путь к config.properties, с ним дальше и работаем
class ConfigService(val file: Path):
  // из строки получаем путь, относительные пути считаем от папки настроек
  // этот метод вызывается из меню, когда пользователь вводит путь к папке
  def userPath(value: String): Result[Path] =
    if value.isBlank then Left(AppError.BadPath(value))
    else
      // если путь начинается с ~, подставляем домашнюю папку пользоватлея
      val expanded = if value == "~" || value.startsWith("~/") then
        System.getProperty("user.home") + value.drop(1)
      else value
      // полный путь уже готов, а например demo надо добавить к папке config.properties
      Result.io(file.getParent.resolve(Paths.get(expanded)).normalize()).left.map(_ => AppError.BadPath(value))

  // вызывается в Main: сначала читаем файл, потом собираем настройки программы
  def load(): Result[AppConfig] =
    // ## сперва проверяем путь, потом читаем файл и разбираем настройки
    FileService.checkNoLinks(file).flatMap(_ => Result.io(PropertyFiles.read(file))).flatMap(parse)

  // p содержит пары вроде maximumScanDepth=30, из них создаём наши объекты
  private def parse(properties: Properties): Result[AppConfig] =
    // эти методы читают из настроек строки, числа, списки и т.д.
    val f = new Fields(properties)
    val defaultProfile = f.text("defaultProfile", "safe")
    def path(key: String) = f.required(key).flatMap(userPath)

    def root(id: String, depth: Int): Result[SearchRoot] =
      // для demo получаем root.demo. и дальше дописываем name, path и другие поля
      val k = s"root.$id."
      for
        folder <- path(k + "path")
        limit <- f.integer(k + "depth", depth)
        enabled <- f.bool(k + "enabled", true)
      yield SearchRoot(id, f.text(k + "name", id), folder, f.text(k + "profile", defaultProfile),
        limit, enabled, f.list(k + "exclusions"))

    // тут собираются правила
    def rule(id: String): Result[Rule] =
      val k = s"rule.$id."
      for
        category <- f.required(k + "category")
        age <- f.number(k + "minimumAgeHours", 0)
        min <- f.number(k + "minimumSize", 0)
        max <- f.optionalSize(k + "maximumSize")
        days <- f.days(k + "retentionDays")
        priority <- f.integer(k + "priority", 0)
        enabled <- f.bool(k + "enabled", true)
      yield Rule(id, f.text(k + "name", id), f.optional(k + "description"), category,
        f.text(k + "type", "file"), f.optional(k + "exactName"), f.list(k + "patterns"),
        f.optional(k + "regex"), f.optional(k + "extension"), f.optional(k + "relativePattern"),
        f.list(k + "parentContains"), f.list(k + "contains"), age, min, max, days, priority, enabled)

    // профиль full и остальные получают свои правила, категории и ограниченя
    def profile(id: String): Result[Profile] =
      val k = s"profile.$id."
      for
        days <- f.days(k + "retentionDays")
        max <- f.optionalSize(k + "maximumSize")
      yield Profile(id, f.list(k + "rules"), f.list(k + "categories"), f.list(k + "excludedRules"), days, max)

    // читаем исключения, например fixtures и *.keep.tmp из учебного примера
    def exclusion(id: String): Result[Exclusion] =
      val k = s"exclusion.$id."
      for
        kind <- f.required(k + "kind")
        raw <- f.required(k + "value")
        // у исключений по пути тоже нужно получить полный путь
        value <- if Set("tree", "path").contains(kind) then userPath(raw).map(_.toString) else Right(raw)
      yield Exclusion(id, kind, value, f.optional(k + "scope"))

    for
      // переходы по ссылкам эта версия не поддерживает
      follow <- f.bool("followSymbolicLinks", false)
      _ <- Result.check(!follow, AppError.BadConfig("Переход по ссылкам запрещён"))
      depth <- f.integer("maximumScanDepth", 30)
      days <- f.integer("defaultRetentionDays", 14)
      ask <- f.bool("askAboutExpiredOnStartup", true)
      fullPaths <- f.bool("logFullPaths", false)
      quarantine <- path("quarantineDirectory")
      state <- path("stateFile")
      log <- path("logFile")
      history <- path("historyFile")
      report <- path("reportFile")
      // берём список точек, например demo, и для каждой собираем SearchRoot
      // ## если хоть одна точка описана с ошибкой, весь конфиг не загружаем
      roots <- Result.collect(f.list("roots"))(id => root(id, depth))
      rules <- Result.collect(f.list("rules"))(rule)
      profiles <- Result.collect(f.list("profiles"))(profile)
      exclusions <- Result.collect(f.list("exclusions"))(exclusion)
      // все прочитанные данные складываем в один AppConfig
      config = AppConfig(file, quarantine, state, log, history, report, defaultProfile, days, depth,
        ask, fullPaths, roots, rules, profiles, exclusions, f.list("globalExclusions"))
      // перед возвратом проверяем настройки, при ошибке дальше не идём
      _ <- ConfigCheck.validate(config)
    yield config

  // вызывается из меню когда добавляем, меняем или удаляем точку поиска
  def saveRoots(roots: List[SearchRoot]): Result[AppConfig] =
    for
      _ <- FileService.checkNoLinks(file)
      p <- Result.io(PropertyFiles.read(file))
      // получаем строку с id через ;, в таком виде список хранится в файле
      _ = p.setProperty("roots", roots.map(_.id).mkString(";"))
      // перезаписываем данные каждой точки в настройках
      _ = roots.foreach { r =>
        val k = s"root.${r.id}."
        p.setProperty(k + "name", r.name)
        // путь записываем через /, чтобы обратные слеши не мешали чтению настроек
        p.setProperty(k + "path", r.path.toString.replace('\\', '/'))
        p.setProperty(k + "profile", r.profile)
        p.setProperty(k + "depth", r.depth.toString)
        p.setProperty(k + "enabled", r.enabled.toString)
        p.setProperty(k + "exclusions", r.exclusions.mkString(";"))
      }
      // сначала проверяем новые настойки, только потом сохраняем в файл
      config <- parse(p)
      _ <- Result.io(PropertyFiles.save(file, p))
    yield config
