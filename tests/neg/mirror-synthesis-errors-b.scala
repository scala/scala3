import scala.deriving.Mirror

sealed trait Lst[+A] // AKA: scala.collection.immutable.List
case class Cns[+A](head: A, tail: Lst[A]) extends Lst[A]
case object Nl extends Lst[Nothing]

sealed trait Opt[+A] // AKA: scala.Option
case class Sm[+A](value: A) extends Opt[A]
case object Nn extends Opt[Nothing]

enum Foo:
  case A, B

object Bar:
  val A: Foo.A.type = Foo.A // alias of Foo.A
  type A = Foo.A.type // type alias

case object Baz
type Baz = Baz.type

val testA = summon[Mirror.ProductOf[Cns[Int] & Sm[Int]]] // error: unreleated
val testB = summon[Mirror.ProductOf[Sm[Int] & Cns[Int]]] // error: unreleated
val testC = summon[Mirror.Of[Cns[Int] & Sm[Int]]] // error: unreleated
val testD = summon[Mirror.Of[Sm[Int] & Cns[Int]]] // error: unreleated
val testE = summon[Mirror.ProductOf[Sm[Int] & Nn.type]] // error: unreleated
val testF = summon[Mirror.ProductOf[Nn.type & Sm[Int]]] // error: unreleated
val testG = summon[Mirror.Of[Foo.A.type & Foo.B.type]] // error: unreleated
val testH = summon[Mirror.Of[Foo.B.type & Foo.A.type]] // error: unreleated
val testI = summon[Mirror.Of[Foo.A.type & Bar.A.type]] // ok
val testJ = summon[Mirror.Of[Bar.A.type & Foo.A.type]] // ok
val testK = summon[Mirror.Of[Foo.A.type & Bar.A.type & Bar.A]] // ok
val testL = summon[Mirror.Of[Baz & Baz.type]] // ok
