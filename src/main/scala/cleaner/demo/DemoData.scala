package cleaner.demo

import cleaner.model.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.nio.file.attribute.FileTime

// Отдельная маленькая программа для создания безопасного примера.
object DemoData {
  def main(args: Array[String]): Unit = {
    val base = Paths.get("demo/sample").toAbsolutePath.normalize()
    if Files.exists(base) then
      println("Папка demo/sample уже существует.")
      return
    val files = List("build.sbt", "target/result.txt", "notes.tmp", "video.part",
      "important.keep.tmp", "fixtures/test.tmp", "build/my-work.txt", ".cache/my-data.txt")
    val written = Result.io {
      for (name <- files) {
        val path = base.resolve(name)
        Files.createDirectories(path.getParent)
        Files.write(path, ("Учебный файл: " + name).getBytes(StandardCharsets.UTF_8))
        Files.setLastModifiedTime(path, FileTime.fromMillis(System.currentTimeMillis() - 5L * 86400000L))
      }
    }
    // ## сообщение о готовности печатаем только если все учебные файлы записались
    written match
      case Left(error) => println(error.message)
      case Right(_) => println("Пример создан. Профиль full найдёт target, notes.tmp, video.part.")
  }
}
