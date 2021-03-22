import scala.deriving.*

sealed trait A

case class C() extends A

sealed trait B extends A

case class D() extends B

@main def Test =
  summon[Mirror.Of[A]]
  summon[Mirror.Of[B]]
