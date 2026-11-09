package cleaner.storage

import cleaner.model.*
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*

// общие действия с файлами, используются при поиске и работе с карантином
object FileService:
  // при проверках не переходим по символическим ссылкам
  private val noLinks = LinkOption.NOFOLLOW_LINKS

  // проверяем существует ли объект по этому пути
  def exists(path: Path): Boolean = Files.exists(path, noLinks)

  // постепенно поднимаемся от объекта к корню диска
  // ## напр для demo/sample проверим сам sample, папку demo и всех родителей выше
  def pathAndParents(path: Path): List[Path] =
    Option(path.getParent) match
      // ## дошли до корня, выше папок уже нет
      case None => List(path)
      // получаем родительскую папку для следующей проверки
      case Some(parent) => path :: pathAndParents(parent)

  // проверяем сам объект и все папки выше него, чтобы не пройти через ссылку
  def checkNoLinks(path: Path): Result[Unit] =
    val parts = pathAndParents(path.toAbsolutePath.normalize())
    Result.collect(parts) { p =>
      if !exists(p) then Right(())
      else
        Result.io {
          // читаем сведения об объекте, содержимое файла здесь не читается
          val a = Files.readAttributes(p, classOf[BasicFileAttributes], noLinks)
          // сравниваем реальный путь с переходом по ссылкам и без перехода
          !a.isSymbolicLink && !a.isOther && p.toRealPath() == p.toRealPath(noLinks)
        }.flatMap(ok => Result.check(ok, AppError.UnsafeFile(p.toString)))
    }.map(_ => ())

  // возвращаем только непосредственные файлы и папки внутри path
  // глубже этот метод сам не заходит, это делает Scanner
  def children(path: Path): Result[List[Path]] = Result.io {
    val stream = Files.newDirectoryStream(path)
    // добавляем каждый найденный путь в список
    // возвращаем обычный список, отсортированный по строке пути
    try stream.iterator().asScala.toList.sortBy(_.toString)
    // закрываем доступ к каталогу даже если случилась ошибка
    finally stream.close()
  }

  // собираем сам объект и всё его содержимое, включая вложенные папки
  // используется для подсчёта размера, проверки и удаления
  def tree(path: Path): Result[List[Path]] =
    // если поток попросили прервать, прекращаем обход
    if Thread.currentThread().isInterrupted then Left(AppError.Interrupted)
    else checkNoLinks(path).flatMap { _ =>
      if Files.isDirectory(path, noLinks) then
        for
          entries <- children(path)
          // продолжаем обход содержимого папки
          branches <- Result.collect(entries)(tree)
        // ## собрали списки из вложенных папок, flatten склеивает их в один
        yield path :: branches.flatten
      // разрешаем обычные файлы, специальные объекты не берём
      else if Files.isRegularFile(path, noLinks) then Right(List(path))
      else Left(AppError.UnsafeFile(path.toString))
    }

  // проверяем подходит ли имя или путь под маску
  // например notes.tmp под *.tmp или папка fixtures под её исключение
  def matches(mask: String, value: String): Boolean =
    val matcher = FileSystems.getDefault.getPathMatcher("glob:" + mask)
    // приводим разделители пути к одному виду
    val name = Paths.get(value.replace('\\', '/'))
    matcher.matches(name) ||
      // убираем **/ и проверяем ещё раз
      // это нужно для fixtures прямо внутри точки поиска
      (mask.startsWith("**/") && matches(mask.drop(3), value)) ||
      // проверяем также саму папку, а не только то что внутри неё
      (mask.endsWith("/**") && matches(mask.dropRight(3), value))

  // получаем FileInfo для кандидата, например нашего notes.tmp
  // перед перемещением этот метод вызывается ещё раз для сравнения
  def info(path: Path): Result[FileInfo] =
    tree(path).flatMap { paths => Result.io {
      // SHA-256 делает строку-отпечаток из собранных сведений об объектах
      val digest = MessageDigest.getInstance("SHA-256")
      var size = 0L
      // сортируем объекты, чтобы порядок обхода не менял отпечаток
      for p <- paths.sortBy(p => path.relativize(p).toString) do
        val a = Files.readAttributes(p, classOf[BasicFileAttributes], noLinks)
        // складываем размеры файлов, у папок размер отдельно не прибавляем
        // addExact выдаст ошибку, если сумма не помещается в Long
        if a.isRegularFile then size = Math.addExact(size, a.size())
        val bytes = if a.isDirectory then 0L else a.size()
        // в строку попадают относительный путь, тип, размер и дата изменения
        // содержимое пользовательских файлов для отпечатка не читаем
        val line = s"${path.relativize(p)}|${a.isDirectory}|$bytes|${a.lastModifiedTime().toMillis}\n"
        // добавляем сведения очередного объекта в расчёт отпечатка
        digest.update(line.getBytes(StandardCharsets.UTF_8))
      // отдельно берём тип и дату самого объекта, который проверяем
      val a = Files.readAttributes(path, classOf[BasicFileAttributes], noLinks)
      FileInfo(size, a.lastModifiedTime().toMillis, a.isDirectory,
        // байты отпечатка переводим в строку из шестнадцатеричных цифр
        digest.digest().map(b => "%02x".format(b & 255)).mkString)
    }}

  // перемещаем объект, например из demo в карантин или обратно
  def move(source: Path, target: Path): Result[Unit] =
    for
      _ <- checkNoLinks(source)
      _ <- checkNoLinks(target)
      // существующий объект по пути назначения не перезаписываем
      _ <- Result.check(!exists(target), AppError.Occupied(target.toString))
      // в этой версии перенос разрешён только внутри одной файловой системы
      sameDisk <- Result.io(Files.getFileStore(source) == Files.getFileStore(target.getParent))
      _ <- Result.check(sameDisk, AppError.DifferentDisk)
      // ## сюда попадём только если путь свободен и обе папки на одном диске
      _ <- Result.io(Files.move(source, target))
    yield ()

  // окончательно удаляем объект и всё что внутри него
  // подтверждение пользоватлея запрашивается в меню до вызова удаления
  def deleteTree(path: Path): Result[Unit] =
    // сначало собираем и проверяем всё дерево
    // если эта проверка завершится ошибкой, до удаления не дойдём
    tree(path).flatMap(paths => Result.collect(paths.reverse) { p =>
      // идём в обратном порядке: сначала содержимое, потом родительские папки
      checkNoLinks(p).flatMap(_ => Result.io(Files.delete(p)))
    }).map(_ => ())
