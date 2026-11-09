package cleaner.search

import cleaner.model.*
import cleaner.storage.FileService
import java.nio.file.{Files, LinkOption, Path, Paths}

class RuleMatcher(config: AppConfig):
  // possible быстро отсеивает правила которые не подходят по типу, имени или пути
  // после него matches проверяет оставшиеся условия, включая возраст и размер
  def possible(rule: Rule, path: Path, root: SearchRoot): Boolean =
    val name = path.getFileName.toString
    val dir = Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS)
    (rule.targetType == "any" || (rule.targetType == "directory") == dir) &&
      // ## если точное имя не задано, forall пропускает эту проверку
      rule.exactName.forall(_ == name) &&
      (rule.patterns.isEmpty || rule.patterns.exists(FileService.matches(_, name))) &&
      rule.regex.forall(name.matches) && rule.extension.forall(name.endsWith) &&
      rule.relativePattern.forall(FileService.matches(_, root.path.relativize(path).toString))

  // проверяем есть ли в папке хотя бы один объект под заданные маски
  // например для target проверяем наличие build.sbt или pom.xml рядом
  private def containsAny(folder: Path, masks: List[String]): Result[Boolean] =
    if masks.isEmpty then Right(true)
    else if !Files.isDirectory(folder) then Right(false)
    else FileService.children(folder).map(_.exists { p =>
      Files.isRegularFile(p, LinkOption.NOFOLLOW_LINKS) && masks.exists(FileService.matches(_, p.getFileName.toString))
    })

  // проверяем конкретный объект по всем условиям правила
  // например notes.tmp должен подходить под маску и быть старше 24 часов
  // если хоть одно заданное условие не выполнено, возвращаем false
  // если все проверки пройдены, возвращаем true
  def matches(rule: Rule, path: Path, root: SearchRoot, info: FileInfo): Result[Boolean] =
    val age = (System.currentTimeMillis() - info.modified) / 3600000L
    if !possible(rule, path, root) || age < rule.minimumAgeHours ||
        // ## right с false значит файл просто не подошёл, ошибку чтения вернём отдельно
        info.size < rule.minimumSize || rule.maximumSize.exists(info.size > _) then Right(false)
    else
      for
        parent <- containsAny(path.getParent, rule.parentContains)
        inside <- containsAny(path, rule.contains)
      yield parent && inside

  // собираем объяснение из условий правила
  // это объяснение потом показывается в подробностях найденного объекта
  def reason(r: Rule): String =
    val parts = List(r.description, r.exactName.map("имя = " + _),
      Option.when(r.patterns.nonEmpty)("маска: " + r.patterns.mkString(", ")),
      r.regex.map("выражение: " + _), r.extension.map("расширение: " + _),
      r.relativePattern.map("путь: " + _),
      Option.when(r.parentContains.nonEmpty)("рядом есть " + r.parentContains.mkString(" или ")),
      Option.when(r.contains.nonEmpty)("внутри есть " + r.contains.mkString(" или ")),
      Some(s"возраст от ${r.minimumAgeHours} ч."), Some(s"размер от ${r.minimumSize} байт"), r.maximumSize.map(n => s"размер до $n байт"))
    // ## flatten оставляет только заданные условия, из них получится объяснение для меню
    parts.flatten.filter(_.nonEmpty).mkString("; ")

  // проверяем защищён ли путь от помещения в карантин
  // защищаем корень диска, домашнюю папку, сами точки поиска и служебные пути
  def protectedPath(path: Path): Boolean =
    val home = Paths.get(System.getProperty("user.home")).toAbsolutePath.normalize()
    path == path.getRoot || path == home || config.roots.exists(_.path == path) ||
      config.technicalPaths.exists(p => path.startsWith(p) || p.startsWith(path))

  // проверяем нужно ли пропустить объект по исключениям
  // rule передаётся когда проверяем исключение по правилу или категории
  def excluded(path: Path, root: SearchRoot, rule: Option[Rule] = None): Result[Boolean] = Result.io {
    val ids = config.globalExclusions ++ root.exclusions
    config.exclusions.filter(e => ids.contains(e.id)).exists { e =>
      // сначала проверяем сам файл или папку
      def applies(p: Path): Boolean =
        val relative = root.path.relativize(p).toString
        val name = Option(p.getFileName).map(_.toString)
        e.scope.forall(FileService.matches(_, relative)) && (e.kind match
          case "path" => p == Paths.get(e.value)
          case "tree" => p.startsWith(Paths.get(e.value))
          case "name" => name.contains(e.value)
          case "mask" => FileService.matches(e.value, relative) || name.exists(FileService.matches(e.value, _))
          case "extension" => name.exists(_.endsWith(e.value))
          case "category" => rule.exists(_.category == e.value)
          case "rule" => rule.exists(_.id == e.value)
          case "youngerHours" => (System.currentTimeMillis() - Files.getLastModifiedTime(p).toMillis) / 3600000L < e.value.toLong
          case _ => false)
      // если исключена папка, её содержимое тоже не трогаем
      // проверяем и родителей точки поиска, чтобы нельзя было обойти .git
      if Set("path","tree","name","mask").contains(e.kind) then
        FileService.pathAndParents(path).exists(applies)
      else applies(path)
    }
  }

  // проверяем безопасность объекта и всего его содержимого
  // при нарушении останавливаем обработку этого объекта с сообщением
  def checkSafe(path: Path, root: SearchRoot, rule: Rule): Result[Unit] =
    def checkItem(p: Path): Result[Unit] =
      for
        _ <- Result.check(!protectedPath(p), AppError.Protected(p.toString))
        skip <- excluded(p, root, Some(rule))
        _ <- Result.check(!skip, AppError.Excluded(p.toString))
        _ <- Result.check(Files.isReadable(p), AppError.NoAccess(p.toString))
        info <- FileService.info(p)
        _ <- Result.collect(config.rules.filter(_.enabled)) { r =>
          matches(r, p, root, info).flatMap { matched =>
            if !matched then Right(())
            else excluded(p, root, Some(r)).flatMap(skip => Result.check(!skip, AppError.Excluded(p.toString)))
          }
        }
      yield ()
    for
      _ <- Result.check(path.startsWith(root.path) && path != root.path, AppError.Protected(path.toString))
      _ <- FileService.checkNoLinks(root.path)
      tree <- FileService.tree(path)
      _ <- Result.collect(tree)(checkItem)
    yield ()
