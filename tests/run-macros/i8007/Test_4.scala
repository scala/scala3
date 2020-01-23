import Macro1._
import Macro2._
import Macro3._
import scala.deriving._

case class Person(name: String, age: Int)

enum Opt[+T] derives Eq {
  case Sm(t: T)
  case Nn
}

@main def Test() = {
  import Opt._

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

  val t5 = test3(Sm(Person("Test", 23)), Sm(Person("Test", 23)))
  println(t5) // true
  println
}