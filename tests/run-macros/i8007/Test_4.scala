import Macro1.*
import Macro2.*
import Macro3.*
import Macro3.eqGen

case class Person(name: String, age: Int)

enum Opt[+T] {
  case Sm(t: T)
  case Nn
}

enum OptInv[+T] {
  case Sm[T](t: T) extends OptInv[T]
  case Nn
}

@main def Test() = {
  import Opt.*
  import Eq.{given, *}

  val t1 = test1(Person("Test", 23))
  println(t1)
  println

  val t2 = test2(Person("Test", 23))
  println(t2)
  println

  val t3 = Person("Test", 23) === Person("Test", 23)
  println(t3) // true
  println

  val t4 = Person("Test", 23) === Person("Test", 24)
  println(t4) // false
  println

  val t5 = Sm(23) === Sm(23)
  println(t5) // true
  println

  val t5_2 = OptInv.Sm(23) === OptInv.Sm(23)
  println(t5_2) // true
  println

  val t6 = Sm(Person("Test", 23)) === Sm(Person("Test", 23))
  println(t6) // true
  println

  val t7 = Sm(Person("Test", 23)) === Sm(Person("Test", 24))
  println(t7) // false
  println
}
