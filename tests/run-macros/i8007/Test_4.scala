import Macro1._
import Macro2._
import Macro3._

@main def Test() = {
  val t1 = test1(Person("Test", 23))
  println(t1)
  println

  val t2 = test2(Person("Test", 23))
  println(t2)
  println

  val t3 = test3(Person("Test", 23), Person("Test", 23))
  println(t3) // true
  println

  val t4 = test3(Person("Test", 23), Person("Test", 24))
  println(t4) // false
  println
}