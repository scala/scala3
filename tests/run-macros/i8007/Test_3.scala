import Macro1._
import Macro2._

@main def Test() = {
  val list = test1(Person("Test", 23))
  println(list)
  println

  test2(Person("Test", 23))
}