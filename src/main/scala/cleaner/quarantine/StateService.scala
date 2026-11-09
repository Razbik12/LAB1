package cleaner.quarantine

import cleaner.model.*
import cleaner.storage.*
import java.nio.file.{Path, Paths}
import java.util.Properties

//сохраняет записи карантина между запусками программы.
// загружает записи карантина из файла и сохраняет их изменения
// save превращает записи обратно в текст и записывает на диск
// update добавляет или заменяет одну запись, remove убирает запись по номеру
// поэтому карантин сохраняется между запусками программы
class StateService(config: AppConfig):
  private var loaded: Result[List[QuarantineEntry]] = read()
  def entries: Result[List[QuarantineEntry]] = loaded

  def verifySaved(entry: QuarantineEntry): Result[Unit] =
    // ## ещё раз читаем файл с диска и проверяем что запись реально сохранилась
    loaded = read().flatMap { items =>
      Result.check(items.contains(entry), AppError.DamagedState("Нет сохранённой записи " + entry.id)).map(_ => items)
    }
    loaded.map(_ => ())

  private def read(): Result[List[QuarantineEntry]] =
    if !FileService.exists(config.stateFile) then Right(Nil)
    else
      val result = for
        _ <- FileService.checkNoLinks(config.stateFile)
        p <- Result.io(PropertyFiles.read(config.stateFile))
        f = new Fields(p)
        version <- f.required("version")
        _ <- Result.check(version == "1", AppError.BadConfig("Версия состояния"))
        ids = f.list("entries")
        count <- f.required("count").flatMap(v => v.toIntOption.toRight(AppError.BadNumber(v)))
        _ <- Result.check(count == ids.size && ids == ids.distinct, AppError.BadConfig("Список записей"))
        entries <- Result.collect(ids)(id => decode(p, f, id))
      yield entries
      // ## если файл испорчен, сохраняем ошибку вместо списка, менять карантин уже нельзя
      result.left.map(e => AppError.DamagedState(e.message))

  private def decode(p: Properties, f: Fields, id: String): Result[QuarantineEntry] =
    // без этих полей нельзя надёжно восстановить запись
    def field(key: String) = f.required(id + "." + key)
    def number(key: String) = field(key).flatMap(v => v.toLongOption.toRight(AppError.BadNumber(v)))
    def path(key: String) = field(key).flatMap(v => Result.io(Paths.get(v)))
    for
      _ <- Result.check(id.matches("[0-9a-f-]{36}"), AppError.BadConfig("Номер записи"))
      original <- path("original")
      root <- path("root")
      name <- field("name")
      _ <- Result.check(original.isAbsolute && original.normalize() == original && root.isAbsolute &&
        root.normalize() == root && original.startsWith(root) && original != root &&
        Option(original.getFileName).exists(_.toString == name), AppError.BadPath(original.toString))
      directory <- field("directory").flatMap(v => v.toBooleanOption.toRight(AppError.BadBoolean(v)))
      size <- number("size")
      moved <- number("moved")
      days <- field("days").flatMap(v => v.toIntOption.toRight(AppError.BadNumber(v)))
      expires <- number("expires")
      rule <- field("rule")
      category <- field("category")
      fingerprint <- field("fingerprint")
      key <- field("status")
      // ## в файле статус записан словом, тут получаем из него один из вариантов EntryStatus
      status <- EntryStatus.values.find(_.key == key).toRight(AppError.BadConfig("Состояние записи: " + key))
      _ <- Result.check(size >= 0 && days >= 0 && moved >= 0 && expires >= moved &&
        fingerprint.matches("[0-9a-f]{64}"),
        AppError.BadConfig("Метаданные записи"))
      _ <- Result.check(p.containsKey(id + ".restoreTarget"), AppError.BadConfig("Нет поля restoreTarget"))
      target <- Result.io(f.optional(id + ".restoreTarget").map(Paths.get(_)))
      _ <- Result.check(status != EntryStatus.Restoring || target.exists(_.isAbsolute), AppError.BadConfig("Нет пути восстановления"))
    yield QuarantineEntry(id, original, root, name, directory, size, moved, days, expires,
      rule, category, fingerprint, status, target)

  private def save(items: List[QuarantineEntry]): Result[Unit] =
    val p = new Properties()
    p.setProperty("version", "1")
    p.setProperty("entries", items.map(_.id).mkString(";"))
    p.setProperty("count", items.size.toString)
    for e <- items do
      val fields = Map("original" -> e.original.toString, "root" -> e.root.toString,
        "name" -> e.name, "directory" -> e.isDirectory.toString, "size" -> e.size.toString,
        "moved" -> e.movedAt.toString, "days" -> e.retentionDays.toString, "expires" -> e.expiresAt.toString,
        "rule" -> e.rule, "category" -> e.category, "fingerprint" -> e.fingerprint, "status" -> e.status.key,
        // ## в старом формате пустой путь записан пустой строкой, в самой программе храним none
        "restoreTarget" -> e.restoreTarget.fold("")(_.toString))
      fields.foreach((key, value) => p.setProperty(e.id + "." + key, value))
    for
      _ <- FileService.checkNoLinks(config.stateFile)
      _ <- Result.io(PropertyFiles.save(config.stateFile, p))
    yield loaded = Right(items)

  def update(entry: QuarantineEntry): Result[Unit] =
    entries.flatMap(items => save(items.filterNot(_.id == entry.id) :+ entry))

  def remove(id: String): Result[Unit] =
    entries.flatMap(items => save(items.filterNot(_.id == id)))
