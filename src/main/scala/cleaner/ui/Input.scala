package cleaner.ui

import cleaner.model.*
import scala.io.StdIn

object Input:
  // задаём вопрос и читаем ответ из консоли
  def ask(question: String): Result[String] =
    print(question + ": ")
    // null означает что ввод закончился, а не просто нажали enter
    // убираем пробелы в начале и конце ответа
    Option(StdIn.readLine()).map(_.trim).toRight(AppError.EndOfInput)

  // ## enter и конец ввода это разные вещи, enter здесь даёт none внутри right
  def optional(question: String): Result[Option[String]] = ask(question).map(s => Option.when(s.nonEmpty)(s))
  def number(s: String): Result[Int] = s.toIntOption.toRight(AppError.BadNumber(s))
  // ## сначало читаем ответ, если он есть — пробуем получить число
  def integer(question: String): Result[Int] = ask(question).flatMap(number)
  // из ответа вроде 1,2,3 получаем список отдельных строк
  def ids(s: String): List[String] = s.split(",").toList.map(_.trim).filter(_.nonEmpty).distinct
  // подтверждение принимается только если написали ДА
  def confirm(question: String): Result[Boolean] = ask(question + " Напишите ДА").map(_ == "ДА")

  def choice[A](question: String, options: (String, A)*): Result[A] =
    ask(question).flatMap(s => options.toMap.get(s).toRight(AppError.UnknownCommand(s)))

  // ## слова из консоли разбираем здесь один раз, дальше пойдёт готовый выбор
  def selection(): Result[Selection] =
    ask("Выбор: ids, category, rule, all, clear").flatMap {
      case "ids" => ask("Номера через запятую").flatMap(s => Result.collect(ids(s))(number)).map(Selection.Ids.apply)
      case "category" => ask("Категория").map(Selection.Category.apply)
      case "rule" => ask("Правило").map(Selection.Rule.apply)
      case "all" => Right(Selection.All)
      case "clear" => Right(Selection.Clear)
      case s => Left(AppError.UnknownCommand(s))
    }
