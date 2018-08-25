trait Animal
object Animal {
  def enumValues: Iterable[Animal] = Dog.enumValues ++ Cat.enumValues
  def enumValueNamed = Dog.enumValueNamed ++ Cat.enumValueNamed
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
    assert(Animal.enumValueNamed("Boxer") == Boxer)
    assert(Animal.enumValueNamed("Ragdoll") == Ragdoll)
  }
}
