package namedparams

class C[type Elem, type Value](val elem: Elem) {
  def toVal: Elem = ???
}

abstract class D[type Elem, V](elem: Elem) extends C[Elem, V](elem)

object Test {
  val c = new C[String, String]("A") {
    override def toVal = elem
  }
  val x: c.Elem = c.elem

  val c2: C { type Elem = String } = c

  val c3 = new C[Elem = String, Value = Int]("B")
  val c4 = new C[Elem = String]("C")
  val x2: c2.Elem = c2.elem
}

// Adapated from i94-nada
trait Test1 {
  trait Monad[type Elem] {
    def unit: Elem
  }
  sealed abstract class Either[A,B]
  case class Left[A,B](unit: A) extends Either[A,B] with Monad[A]
  case class Right[A,B](unit: B) extends Either[A,B] with Monad[B]
  def flatMap[X,Y,M <: Monad](m: M[Elem = X], f: X => M[Elem = Y]): M[Elem = Y] = f(m.unit)
  println(flatMap(Left(1), {x: Int => Left(x)}))
}
