package cleaner.model

// профиль определяет какие правила применять при поиске
// например editors ищет временные файлы редакторов,
// full включает все категории
case class Profile(
  id: String,
  rules: List[String],
  categories: List[String],
  excludedRules: List[String],
  // ## свой срок можно пропустить, тогда при поиске возьмём срок из правила
  retentionDays: Option[Int],
  maximumSize: Option[Long]
)
