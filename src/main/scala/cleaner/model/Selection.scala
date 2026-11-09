package cleaner.model

// ## здесь уже лежит выбор с данными, напр номера файлов или название категории
enum Selection:
  case Ids(numbers: List[Int])
  case Category(name: String)
  case Rule(id: String)
  case All, Clear

  def select(items: List[Candidate]): Result[List[Candidate]] = this match
    // выбираем конкретные номера из результатов поиска
    case Ids(numbers) =>
      // ## если хоть один номер лишний, сразу скажем какой и не будем менять выбор
      numbers.find(id => !items.exists(_.id == id)) match
        case Some(id) => Left(AppError.UnknownCandidate(id))
        case None => Right(items.filter(c => numbers.contains(c.id)))
    // выбираем все объекты одной категории
    case Category(name) => Right(items.filter(_.rule.category == name))
    // выбираем объекты с этим основным правилом
    case Rule(id) => Right(items.filter(_.rule.id == id))
    case All => Right(items)
    case Clear => Right(Nil)
