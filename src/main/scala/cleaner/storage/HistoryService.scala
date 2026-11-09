package cleaner.storage

import cleaner.model.*
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}
import java.time.Instant
import java.util.UUID
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

// записывает события программы в историю и журнал
class HistoryService(config: AppConfig) {
  // события текущего запуска, ReportService использует их для отчёта
  val session = ListBuffer.empty[String]
  private val errors = ListBuffer.empty[AppError]

  // ## меню забирает ошибки записи, очищаем список чтоб не показать их дважды
  def takeErrors(): List[AppError] =
    val result = errors.toList
    errors.clear()
    result

  def errorDetails(error: AppError): String =
    // ## если полные пути выключены, пишем в журнал только название ошибки
    if config.logFullPaths then error.message else error.productPrefix

  // записываем событие, например начало поиска или перенос в карантин
  // если статус не передали, используем OK
  def record(action: String, details: String, status: String = "OK",
             operationId: String = UUID.randomUUID().toString): Unit =
    // одна запись содержит пять полей:
    // время, уникальный номер, действие, статус и подробности
    // поля разделяем табуляцией
    val line = List(Instant.now().toString, operationId, action, status, details)
      // убираем табуляцию и переносы строк из текста
      // иначе одно событие может случайно занять несколько строк или колонок
      .map(_.replace('\t', ' ').replace('\n', ' ').replace('\r', ' ')).mkString("\t")
    // сохраняем событие в памяти для отчёта текущего запуска
    session += line
    // одну и ту же запись добавляем в историю и в журнал
    for file <- List(config.historyFile, config.logFile) do
      val written = for
        // проверяем что путь не проходит через ссылку
        _ <- FileService.checkNoLinks(file)
        _ <- Result.io {
          // создаём родительскую папку, если её ещё нет
          Files.createDirectories(file.getParent)
          // CREATE создаёт файл если его нет
          // APPEND дописывает в конец, прежние записи сохраняются
          Files.writeString(file, line + System.lineSeparator(), StandardCharsets.UTF_8,
            StandardOpenOption.CREATE, StandardOpenOption.APPEND)
        }
      yield ()
      // ## сбой журнала запоминаем отдельно, сам перенос файла мог уже пройти
      written.left.foreach(errors += _)

  def rows(): Result[List[List[String]]] =
    if !FileService.exists(config.historyFile) then Right(Nil)
    else FileService.checkNoLinks(config.historyFile).flatMap { _ =>
      // открываем файл истории для чтения в UTF-8
      // разделяем запись на пять полей
      // -1 сохраняет пустые поля, даже если они находятся в конце строки
      Result.io(Files.readAllLines(config.historyFile, StandardCharsets.UTF_8).asScala.toList.map(_.split("\t", -1).toList))
    }
  // подготавливаем путь кандидата для записи в журнал
  def path(candidate: Candidate): String = {

    // полный путь пишем только если это включено в настройках
    if (config.logFullPaths) {
      candidate.path.toString
    } else {

      // по умолчанию получаем что-то вроде demo:sample/notes.tmp
      candidate.root.id + ":" + candidate.relative
    }
  }

  // сведения о восстановленной записи, содержимое файлов сюда не попадает
  def restoredDetails(entry: QuarantineEntry, target: java.nio.file.Path): String = {
    val rootId = config.roots.find(_.path == entry.root).map(_.id).getOrElse("original-root")

    // полные пути используем только если это разрешено настройкой
    def display(path: java.nio.file.Path): String = {
      if (config.logFullPaths) path.toString
      else rootId + ":" + entry.root.relativize(path).toString
    }

    List(
      "id=" + entry.id,
      "original=" + display(entry.original),
      "root=" + (if (config.logFullPaths) entry.root.toString else rootId),
      "name=" + entry.name,
      "type=" + (if (entry.isDirectory) "directory" else "file"),
      "bytes=" + entry.size,
      "movedAt=" + Instant.ofEpochMilli(entry.movedAt),
      "days=" + entry.retentionDays,
      "expiresAt=" + Instant.ofEpochMilli(entry.expiresAt),
      "rule=" + entry.rule,
      "category=" + entry.category,
      "fingerprint=" + entry.fingerprint,
      "status=restored",
      "target=" + display(target),
      "restoredAt=" + Instant.now()
    ).mkString("; ")
  }


}
