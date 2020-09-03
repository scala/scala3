import Macro1._
import Macro2._
import Macro3._
import Macro3.eqGen

case class Person(name: String, age: Int)

enum Opt[+T] {
  case Sm[U](t: U) extends Opt[U]
  case Nn
}

enum OptInfer[+T] {
  case Sm[+U](t: U) extends OptInfer[U]
  case Nn
}

// simulation of Opt using case class hierarchy
sealed abstract class OptCase[+T]
object OptCase {
  final case class Sm[T](t: T) extends OptCase[T]
  case object Nn extends OptCase[Nothing]
}

@main def Test() = {
  import Opt._
  import Eq.{given _, _}

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

  val t5 = Opt.Sm[Int](23) === Opt.Sm(23) // same behaviour as case class when using apply
  println(t5) // true
  println

  val t5_2 = OptCase.Sm[Int](23) === OptCase.Sm(23)
  println(t5_2) // true
  println

  val t5_3 = OptInfer.Sm(23) === OptInfer.Sm(23) // covariant `Sm` case means we can avoid explicit type parameter
  println(t5_3) // true
  println

  val t6 = Sm[Person](Person("Test", 23)) === Sm(Person("Test", 23))
  println(t6) // true
  println

  val t7 = Sm[Person](Person("Test", 23)) === Sm(Person("Test", 24))
  println(t7) // false
  println
}
