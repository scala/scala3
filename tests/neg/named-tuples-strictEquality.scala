//> using options -language:strictEquality

object Test:

  val u: (name: String, age: Int) = (name = "Bob", age = 25)
  val v: (name: String, birthYear: Int) = (name = "Charlie", birthYear = 1990)

  val b1: Boolean = u.toTuple == v.toTuple // ok
  val b2: Boolean = u == v // error

  val ScalaBook = (name = "Programming in Scala, 5th edition", published = 2021)

  def hasScalaBook(books: IterableOnce[(name: String, age: Int)]): Boolean =
    books.exists {
      case ScalaBook => true // error
      case _         => false
    }
