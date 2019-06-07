trait Animal
object Animal {
  def enumValues: Iterable[Animal] = Dog.values ++ Cat.values
}

enum Dog extends Animal {
  case GermanShepherd, Labrador, Boxer
}

enum Cat extends Animal {
  case PersianCat, Ragdoll, ScottishFold
}

object Test {
  import Dog._
  import Cat._

  def main(args: Array[String]): Unit = {
    val values = List(
      GermanShepherd,
      Labrador,
      Boxer,
      PersianCat,
      Ragdoll,
      ScottishFold
    )

    assert(Animal.enumValues == values)
  }
}
