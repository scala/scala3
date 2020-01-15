import Macro._

@main def Test() = {
  val list = usingSummonFrom[Person](Person("Test", 23))
  println(list)
}