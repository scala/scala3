//> using options -language:strictEquality

@main def Test =

  val u: (name: String, age: Int) = (name = "Bob", age = 25)
  val v: (name: String, birthYear: Int) = (name = "Charlie", birthYear = 1990)

  val ScalaBook = (name = "Programming in Scala, 5th edition", published = 2021)
  val books = Map(
    ScalaBook,
    (name = "Hands on Scala, 2nd edition", published = 2026),
  )

  assert(u.toTuple != v.toTuple)
  assert(u == u)
  assert(v == v)

  def hasScalaBook(books: IterableOnce[(name: String, published: Int)]): Boolean =
    books.exists {
      case ScalaBook => true
      case _         => false
    }

  assert(hasScalaBook(books))
