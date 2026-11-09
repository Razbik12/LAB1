package cleaner.model

import java.nio.file.Path

//SearchRoot хранит сведения об одной папке поиска.
// Сам ничего не ищет: меню создаёт или показывает эти объекты
// а Scanner использует их при обходе папок
// Одна папка, в которой пользователь разрешил искать временные файлы
case class SearchRoot(
  id: String,
  name: String,
  path: Path,
  profile: String,
  depth: Int,
  enabled: Boolean,
  exclusions: List[String]
)
