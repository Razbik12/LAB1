package cleaner.ui

import cleaner.model.Candidate

import java.time.Instant
// показывает результаты поиска, данные получает из Candidate
class ResultView {
  def show(items: List[Candidate], group: Group, sort: Sort, filter: Option[String]): Unit = {
    // ## если фильтр пустой, оставляем все файлы, иначе проверяем путь или категорию
    val filtered = items.filter(c => filter.forall(s => c.relative.contains(s) || c.rule.category == s))
    val result = sort match {
      case Sort.Size => filtered.sortBy(c => -c.info.size)
      case Sort.Age => filtered.sortBy(_.info.modified)
      case Sort.Category => filtered.sortBy(_.rule.category)
      case Sort.Path => filtered.sortBy(_.path.toString)

    }
    val grouped = group match {
      case Group.Root => result.groupBy(_.root.id)
      case Group.Rule => result.groupBy(_.rule.id)
      case Group.Category => result.groupBy(_.rule.category)

    }
    for ((name, members) <- grouped.toList.sortBy(_._1)) {
      println("\n" + name + ": " + members.size + " объектов, " + members.map(_.info.size).sum + " байт")
      for (c <- members) println(c.id.toString + " | " + c.info.size + " байт | " + c.relative)
    }
    println("Всего: " + result.size + " объектов, " + result.map(_.info.size).sum + " байт")
  }

  def details(c: Candidate): Unit = {
    println("Объект: " + c.id + "\nПуть: " + c.path + "\nТочка: " + c.root.id)
    println("Тип: " + (if (c.info.isDirectory) "каталог" else "файл"))
    println("Размер: " + c.info.size + " байт\nИзменён: " + Instant.ofEpochMilli(c.info.modified))
    println("Категория: " + c.rule.category + "\nПравило: " + c.rule.name)
    println("Другие правила: " + c.otherRules.mkString(", ") + "\nХранить дней: " + c.retentionDays)
    println("Причина: " + c.reason)
    c.warnings.foreach(println)
  }
}
