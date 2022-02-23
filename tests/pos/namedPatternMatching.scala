package patterns

class Age(val hidden: Int)

object Age:
  def apply(years: Int): Age = new Age(years)

  case class UnapplyAge(_1: Int) {
    type _1 = "years"
    inline def _years = _1
    type names = "years" +: ()
  }

  // TODO: Describe alternative encoding with tagged tuples
  def unapply(age: Age): UnapplyAge = UnapplyAge(age.hidden)

// TODO: Doesn't work. indexOfNames doesn't extract the correct names
object StringExample:
  def unapply(str: String): { type _1 = "first"; type _2 = "last"} & Option[(Char, Char)]  =
    Some((str.head, str.last)).asInstanceOf

case class User(name: String, age: Age, city: String)

val user = User(name = "Anna", age = Age(10), city = "Berlin")

val annasCity = user match
  case User(name = "Tom", city = city) => ???
  case User(city = c, name = s"Ann$_") => c
  case User(name = guy @ ("Guy" | "guy")) => ???

// nested patterns
val User(name = name, age = Age(years = years)) = user

// partial funtion
val maybeTom = Some(user).collect {
  case u @ User(name = "Tom") => u
}

val berlinerNames = for
  case User(city = "Berlin", name = name) <- List(user)
yield name

@main
def main(): Unit =
  println(annasCity)
  println(name)
  println(years)

  println(maybeTom)
  println(berlinerNames)

