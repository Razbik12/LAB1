package cleaner.model

import java.nio.file.Path

// ## теперь видно причину пропуска, напр дошли до предела глубины
enum SkipReason:
  case DepthLimit
  case Failed(error: AppError)

case class Skipped(path: Path, reason: SkipReason)

// общий результат поиска, Scanner создаёт его и возвращает в ConsoleMenu
case class ScanResult(
  candidates: List[Candidate],
  checked: Long,
  roots: List[String],
  profiles: List[String],
  skipped: List[Skipped],
  warnings: List[String]
)
