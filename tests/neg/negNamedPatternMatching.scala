package patterns

type Age = Age.Age

object Age:
  opaque type Age = Int

  def apply(years: Int): Age = years

  def unapply(age: Age): Some[Int] =
    Some(age)

case class User(name: String, age: Age, city: String)

val user = User(name = "Anna", age = Age(10), city = "Berlin")

val annasCity = user match
  case User(names = "Tom", city = city) => ??? // error
  case User(city = _, 10) => null // error
  case User(age = Age(years = 10)) => null // error
  case User(
    name = "Tom",
    name = "Tom 2",  // error
    name = "Tom 3" // error
    ) => null
  case User(_, name = "Anna") => null // error