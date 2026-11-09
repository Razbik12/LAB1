package cleaner.model

import java.nio.file.Path

// ## по статусу видно, успели ли мы закончить перенос файла
enum EntryStatus(val key: String):
  case Pending extends EntryStatus("pending")
  case Active extends EntryStatus("active")
  case Restoring extends EntryStatus("restoring")
  case Deleting extends EntryStatus("deleting")

// запись об одном объекте в карантине
// хранит сведения для восстановления, проверки и окончательного удаления
case class QuarantineEntry(
  id: String,
  original: Path,
  root: Path,
  name: String,
  isDirectory: Boolean,
  size: Long,
  movedAt: Long,
  retentionDays: Int,
  expiresAt: Long,
  rule: String,
  category: String,
  fingerprint: String,
  status: EntryStatus,
  // ## путь появится только когда начнём восстановление
  restoreTarget: Option[Path]
)
