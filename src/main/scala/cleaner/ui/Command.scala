package cleaner.ui

// ## это четыре раздела главного меню, у каждого есть подпись для экрана
enum Section(val title: String):
  case Roots extends Section("Точки поиска")
  case Search extends Section("Поиск и результаты")
  case Quarantine extends Section("Карантин")
  case Service extends Section("История и настройки")

// ## каждый case это один пункт, номер пользователь вводит только в меню
enum Command(val section: Section, val title: String):
  // действия с точками поиска
  case ShowRoots extends Command(Section.Roots, "Посмотреть точки")
  case AddRoot extends Command(Section.Roots, "Добавить точку")
  case EditRoot extends Command(Section.Roots, "Изменить точку")
  case RemoveRoot extends Command(Section.Roots, "Удалить точку")
  // действия с поиском и найденными объектами
  case Profiles extends Command(Section.Search, "Посмотреть профили")
  case Scan extends Command(Section.Search, "Начать поиск")
  case Results extends Command(Section.Search, "Посмотреть результаты")
  case Details extends Command(Section.Search, "Подробнее об объекте")
  case Select extends Command(Section.Search, "Выбрать объекты")
  case Move extends Command(Section.Search, "Поместить выбранные объекты в карантин")
  // действия с уже перемещёнными объектами
  case ShowEntries extends Command(Section.Quarantine, "Посмотреть карантин")
  case Restore extends Command(Section.Quarantine, "Восстановить объект")
  // ## это имя команды продления, сам срок поменяется позже в сервисе карантина
  case Extend extends Command(Section.Quarantine, "Продлить срок хранения")
  case Purge extends Command(Section.Quarantine, "Окончательно удалить выбранные записи")
  case Expired extends Command(Section.Quarantine, "Просроченные записи")
  // история, отчёт и перезагрузка настроек
  case History extends Command(Section.Service, "Посмотреть историю")
  case Report extends Command(Section.Service, "Сохранить отчёт")
  case Reload extends Command(Section.Service, "Перезагрузить настройки")

// ## выбираем по чему собрать файлы в группы
enum Group:
  case Root, Category, Rule

// ## этот выбор задаёт порядок файлов внутри группы
enum Sort:
  case Size, Age, Category, Path

// ## варианты на случай если при восстановлении путь уже занят
enum Conflict:
  case Name, Folder, Replace, Cancel

// ## что сделать с записями у которых вышел срок
enum ExpiredAction:
  case ListEntries, DeleteAll, DeleteIds, Extend, Keep
