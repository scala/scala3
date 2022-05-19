import scala.deriving.Mirror

trait A
class B extends A

sealed trait C

sealed abstract class D(i: Int)(s: String)
case class SubD(i: Int)(s: String) extends D(i)(s)

sealed class E

sealed trait F

object Foo {
    sealed trait G
    case class H() extends G
    val J = new G { }
}

val testA = summon[Mirror.Of[A]] // error: Not a sealed trait
val testC = summon[Mirror.Of[C]] // error: Does not have subclasses
val testD = summon[Mirror.Of[D]] // error: child SubD takes more than one parameter list
val testSubD = summon[Mirror.Of[SubD]] // error: takes more than one parameter list
val testE = summon[Mirror.Of[E]] // error: Not an abstract class
val testF = summon[Mirror.Of[F]] // error: No children
val testG = summon[Mirror.Of[Foo.G]] // error: Has anonymous subclasses
