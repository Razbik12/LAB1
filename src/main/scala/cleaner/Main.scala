package cleaner

import cleaner.model.*
import cleaner.config.ConfigService
import cleaner.storage.FileService
import cleaner.ui.ConsoleMenu
import java.nio.channels.FileChannel
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.util.Using

object Main:
  def main(args: Array[String]): Unit =
    // ## шагаем по порядку, если загрузка вернула left, до запуска меню уже не дойдём
    val started = for
      // Если передан арг- берем его, если нет - то испл config. Absolutlypath-полный путь к файлу
      path <- Result.io(Paths.get(args.headOption.getOrElse("config.properties")).toAbsolutePath.normalize())
      //создать читателя настроек
      service = new ConfigService(path)
      // прочитать настрйоки
      config <- service.load()
      // защита от 2 одновремен запусков
      lockPath = config.stateFile.resolveSibling("application.lock")
      _ <- FileService.checkNoLinks(lockPath)
      //создание служ папки
      _ <- Result.io(Files.createDirectories(lockPath.getParent))
      result <- Result.io {
        Using.resource(FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE)) { channel =>
          // ## если файл уже занят другим запуском, вернётся ошибка AlreadyRunning
          Option(channel.tryLock()).toRight(AppError.AlreadyRunning).map { lock =>
            // создание меню и показ
            try new ConsoleMenu(service, config).run()
            finally lock.release()
          }
        }
      }
      _ <- result
    yield ()
    started.left.foreach(error => println("Ошибка запуска: " + error.message))
