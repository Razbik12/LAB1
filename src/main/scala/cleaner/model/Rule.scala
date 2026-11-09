package cleaner.model

// одно правило поиска, например поиск старых временных файлов редактора
// разные условия должны выполняться вместе
// внутри списка масок достаточно одного совпадения
case class Rule(
  id: String,
  name: String,
  description: Option[String],
  category: String,
  targetType: String,
  // ## если точное имя не задано, здесь none и имя отдельно не проверяем
  exactName: Option[String],
  patterns: List[String],
  regex: Option[String],
  extension: Option[String],
  relativePattern: Option[String],
  parentContains: List[String],
  contains: List[String],
  minimumAgeHours: Long,
  minimumSize: Long,
  maximumSize: Option[Long],
  retentionDays: Option[Int],
  priority: Int,
  enabled: Boolean
) {
  // считаем сколько условий задано у правила
  // Scanner использует это при выборе между правилами с одинаковым приоритетом
  def specificity: Int = {
    var count = 0
    if (exactName.nonEmpty) count += 1
    if (patterns.nonEmpty) count += 1
    if (regex.nonEmpty) count += 1
    if (extension.nonEmpty) count += 1
    if (relativePattern.nonEmpty) count += 1
    if (parentContains.nonEmpty) count += 1
    if (contains.nonEmpty) count += 1
    if (minimumAgeHours > 0) count += 1
    if (minimumSize > 0) count += 1
    if (maximumSize.exists(_ < Long.MaxValue)) count += 1
    if (targetType != "any") count += 1
    count
  }
}
