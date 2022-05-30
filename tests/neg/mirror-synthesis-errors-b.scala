import scala.deriving.Mirror

sealed trait Lst[+A] // AKA: scala.collection.immutable.List
case class Cns[+A](head: A, tail: Lst[A]) extends Lst[A]
case object Nl extends Lst[Nothing]

sealed trait Opt[+A] // AKA: scala.Option
case class Sm[+A](value: A) extends Opt[A]
case object Nn extends Opt[Nothing]

val testA = summon[Mirror.ProductOf[Cns[Int] & Sm[Int]]] // error: unreleated
val testB = summon[Mirror.ProductOf[Sm[Int] & Cns[Int]]] // error: unreleated
val testC = summon[Mirror.Of[Cns[Int] & Sm[Int]]] // error: unreleated
val testD = summon[Mirror.Of[Sm[Int] & Cns[Int]]] // error: unreleated
val testE = summon[Mirror.ProductOf[Sm[Int] & Nn.type]] // error: unreleated
val testF = summon[Mirror.ProductOf[Nn.type & Sm[Int]]] // error: unreleated
