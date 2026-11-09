package cleaner.model

import java.nio.file.Path

case class AppConfig(
  file: Path,
  quarantine: Path,
  stateFile: Path,
  logFile: Path,
  historyFile: Path,
  reportFile: Path,
  defaultProfile: String,
  defaultRetentionDays: Int,
  maximumDepth: Int,
  askExpired: Boolean,
  logFullPaths: Boolean,
  roots: List[SearchRoot],
  rules: List[Rule],
  profiles: List[Profile],
  exclusions: List[Exclusion],
  globalExclusions: List[String]
) {

  // использ. в выборе файлов допустим если ввести editors то прога ищет только временные файлы
  def profile(id: String): Result[Profile] =
    // ## find вернёт none, если такого профиля нет, тут превращаем это в ошибку с его id
    profiles.find(_.id == id).toRight(AppError.UnknownProfile(id))

  // выбираем правила для профиля, который пользователь выбрал при поиске
  def rulesFor(p: Profile): List[Rule] = {
    rules.filter { r =>
      // Правило включено и профиль его не исключает
      r.enabled && !p.excludedRules.contains(r.id) &&
        // Правило добавлено в профиль отдельно или через его категорию
        (p.rules.contains(r.id) || p.categories.contains(r.category))
    }
  }

  // служебые пути программы, которые нужно защищать от очистки
  def technicalPaths: List[Path] = {

    // getParent берёт папку файла состояния, чтобы защитить всю папку
    // Scanner использует этот список и пропускает служебные объекты
    List(file, quarantine, stateFile.getParent, logFile, historyFile, reportFile)
  }
}
