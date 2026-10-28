//> using options -Werror

sealed trait Base

case class Leaf1() extends Base

case class Leaf2() extends Base

inline def isParticularChild[T <: Base](value: Base): Boolean = value match {
  case x: T => true
  case _ => false // used to warn: Only null is matched
}

@main def test = {
  val child1: Base = Leaf1()
  val child2: Base = Leaf2()
  println(isParticularChild[Leaf1](child1))
  println(isParticularChild[Leaf1](child2))
  println(isParticularChild[Leaf2](child1))
  println(isParticularChild[Leaf2](child2))
}
