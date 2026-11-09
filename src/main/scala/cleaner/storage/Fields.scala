package cleaner.storage

import cleaner.model.*
import java.util.Properties

// помогает получать из текста строки, числа, списки и логические значения
class Fields(p: Properties):
  def optional(key: String): Option[String] =
    // ## нет ключа или после пробелов пусто — получим none
    Option(p.getProperty(key)).map(_.trim).filter(_.nonEmpty)

  // читаем строку, если ключа нет используем default
  def text(key: String, default: String): String = optional(key).getOrElse(default)
  // читаем список значений разделённых точкой с запятой
  def list(key: String): List[String] =
    optional(key).toList.flatMap(_.split(";")).map(_.trim).filter(_.nonEmpty)

  def required(key: String): Result[String] =
    optional(key).toRight(AppError.BadConfig(s"Нет поля $key"))

  // читаем строку и превращаем её в число Int
  def integer(key: String, default: Int): Result[Int] =
    // ## если в поле буквы вместо числа, вернём ошибку с именем поля
    text(key, default.toString).toIntOption.toRight(AppError.BadConfig(s"Нужно целое число в $key"))

  // читаем строку и превращаем её в число Long
  def number(key: String, default: Long): Result[Long] =
    text(key, default.toString).toLongOption.toRight(AppError.BadConfig(s"Нужно число в $key"))

  def optionalSize(key: String): Result[Option[Long]] =
    optional(key) match
      case None => Right(None)
      case Some(value) => value.toLongOption.filter(_ >= 0).map(Some(_)).toRight(AppError.BadConfig(key))

  // разрешаем только true или false
  def bool(key: String, default: Boolean): Result[Boolean] =
    text(key, default.toString).toBooleanOption.toRight(AppError.BadConfig(s"Нужно true или false в $key"))

  def days(key: String): Result[Option[Int]] =
    optional(key) match
      // ## если срок не задан, это нормально, вернём right с пустым значением
      case None => Right(None)
      case Some(value) => value.toIntOption.filter(_ >= 0).map(Some(_)).toRight(AppError.BadConfig(key))
