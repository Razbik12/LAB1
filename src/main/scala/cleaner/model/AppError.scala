package cleaner.model

import scala.util.control.NonFatal

// ## справа в right лежит результат, слева в left причина почему не получилось
type Result[A] = Either[AppError, A]

// ## каждая причина это свой case, меню может понять что именно пошло не так
enum AppError(val message: String):
  case EndOfInput extends AppError("Ввод завершён")
  case InvalidMenuChoice(reason: String) extends AppError(reason)
  case UnknownCommand(value: String) extends AppError(s"Нет такой команды: $value")
  case BadNumber(value: String) extends AppError(s"Нужно целое число: $value")
  case BadBoolean(value: String) extends AppError(s"Нужно true или false: $value")
  case BadPath(value: String) extends AppError(s"Неверный путь: $value")
  case BadConfig(reason: String) extends AppError(s"Ошибка настроек: $reason")
  case DamagedState(reason: String) extends AppError(s"Состояние повреждено: $reason. Карантин заблокирован.")
  case UnknownProfile(id: String) extends AppError(s"Неизвестный профиль: $id")
  case UnknownRoot(id: String) extends AppError(s"Точка не найдена: $id")
  case DuplicateRoot(id: String) extends AppError(s"Идентификатор точки занят: $id")
  case UnknownCandidate(id: Int) extends AppError(s"Нет объекта с номером $id")
  // ## запоминаем какой id искали, чтобы потом показать его в меню
  case UnknownEntry(id: String) extends AppError(s"Нет записи карантина: $id")
  case EmptySelection extends AppError("Сначала выберите объекты")
  case NoRoots extends AppError("Точки не выбраны")
  case InvalidDays extends AppError("Недопустимый срок хранения")
  case NotActive(id: String) extends AppError(s"Операция с записью $id не завершена")
  case NotExpired(id: String) extends AppError(s"Срок записи $id ещё не истёк")
  case Occupied(path: String) extends AppError(s"Путь уже занят: $path")
  case Protected(path: String) extends AppError(s"Защищённый путь: $path")
  case Excluded(path: String) extends AppError(s"Объект исключён: $path")
  case UnsafeFile(path: String) extends AppError(s"Ссылка или специальный файл: $path")
  case NoAccess(path: String) extends AppError(s"Нет доступа: $path")
  case NoSpace(path: String) extends AppError(s"Недостаточно места: $path")
  case DifferentDisk extends AppError("Карантин и исходный файл должны быть на одном диске")
  case FileChanged(path: String) extends AppError(s"Объект изменился: $path")
  case RuleChanged(path: String) extends AppError(s"Правило больше не подходит: $path")
  case RestartRequired extends AppError("Для изменения пути карантина или состояния перезапустите программу")
  case AlreadyRunning extends AppError("Программа уже работает с этим состоянием")
  case Interrupted extends AppError("Операция прервана")
  case FileError(reason: String) extends AppError(s"Ошибка файловой системы: $reason")

object Result:
  // ## если проверка прошла, идём дальше, иначе вернём переданную ошибку
  def check(ok: Boolean, error: => AppError): Result[Unit] =
    Either.cond(ok, (), error)

  // ## вызов к диску может упасть, тут превращаем такую ошибку в left
  def io[A](action: => A): Result[A] =
    try Right(action)
    catch
      case NonFatal(e) => Left(AppError.FileError(Option(e.getMessage).getOrElse(e.getClass.getSimpleName)))

  // ## собираем ответы в список, после первой ошибки остальные операции уже не вызываем
  def collect[A, B](items: List[A])(run: A => Result[B]): Result[List[B]] =
    items.foldLeft[Result[List[B]]](Right(Nil)) { (done, item) =>
      done.flatMap(list => run(item).map(_ :: list))
    // ## добавляли в начало списка, поэтому в конце возвращаем исходный порядок
    }.map(_.reverse)
