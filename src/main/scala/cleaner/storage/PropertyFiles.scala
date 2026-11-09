package cleaner.storage

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.Properties

// PropertyFiles читает и сохраняет настройки в формате ключ=значение
// при сохранении сначала пишет временный файл, потом заменяет основной
// общие методы для текстовых настроек и состояния карантина
object PropertyFiles {

  // читаем файл и получаем пары ключ=значение
  def read(path: Path): Properties = {
    val reader = Files.newBufferedReader(path, StandardCharsets.UTF_8)

    try {
      val properties = new Properties()

      // разбираем строки файла и сохраняем значения в properties
      properties.load(reader)

      // возвращаем прочитанные настройки
      properties

    } finally {

      // закрываем файл даже если чтение закончилось ошибкой
      reader.close()
    }
  }

  // сохраняем пары ключ=значение в файл
  def save(path: Path, properties: Properties): Unit = {
    Files.createDirectories(path.getParent)

    // сначало записываем в отдельный временный файл рядом с основным
    val temporary = Files.createTempFile(
      path.getParent,
      "save-",
      ".tmp"
    )

    try {
      val writer = Files.newBufferedWriter(
        temporary,
        StandardCharsets.UTF_8
      )

      try {

        // записываем настройки, TemporaryCleaner добавляется как комментарий
        properties.store(writer, "TemporaryCleaner")
      } finally {
        writer.close()
      }

      // заменяем основной файл только после завершения записи
      // атомарная замена не оставляет основной файл записанным наполовину
      Files.move(
        temporary,
        path,
        StandardCopyOption.ATOMIC_MOVE,
        StandardCopyOption.REPLACE_EXISTING
      )

    } finally {

      // убираем временный файл если он ещё остался
      Files.deleteIfExists(temporary)
    }
  }

}
