//> using options  -Wimplausible-patterns
object Test {
  sealed abstract class Foo[T]
  case object Bar1 extends Foo[Int]
  case object Bar2 extends Foo[String]
  case object Bar3 extends Foo[AnyRef]

  def fail4[T <: AnyRef](xx: (Foo[T], Foo[T])) = xx match { // warn match may not be exhaustive
    case (Bar1, Bar1) => () // warn // warn
    case (Bar2, Bar3) => ()
    case (Bar3, _) => ()
  }

}