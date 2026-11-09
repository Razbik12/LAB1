package cleaner.model

import java.nio.file.Path

case class FileInfo(size: Long, modified: Long, isDirectory: Boolean, fingerprint: String)

//Сведения о фвйле, который был найден по правилу
case class Candidate(
  id: Int,
  path: Path,
  root: SearchRoot,
  profile: String,
  relative: String,
  info: FileInfo,
  rule: Rule,
  otherRules: List[String],
  retentionDays: Int,
  reason: String,
  warnings: List[String]
)
