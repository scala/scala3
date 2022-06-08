package patterns

class Age(val hidden: Int)

object Age:
  def apply(years: Int): Age = new Age(years)

  // TODO: Describe alternative encoding with tagged tuples
  def unapply(age: Age): Some[Int & { type Names = "years" *: EmptyTuple }] =
    Some(age.hidden.asInstanceOf)

object StringExample:
  def unapply(str: String): Option[(Char, Char) & { type Names = "first" *: "last" *: EmptyTuple }]  =
    Some((str.head, str.last)).asInstanceOf

case class User(name: String, age: Age, city: String)

val user = User(name = "Anna", age = Age(10), city = "Berlin")

val annasCity = user match
  case User(names = "Tom", city = city) => ??? // error
  case User(city = _, 10) => null // error
  case User(
    name = "Tom",
    name = "Tom 2",  // error
    name = "Tom 3" // error
    ) => null