package cleaner.model

// хранит исключение, например папку fixtures которую не нужно очищать
// ## если scope не указан, исключение действует во всей точке поиска
case class Exclusion(id: String, kind: String, value: String, scope: Option[String])
