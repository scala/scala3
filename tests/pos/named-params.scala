package namedparams

class C[type Elem, type Value](val elem: Elem) {
  def toVal: Elem = ???
}

class D[type Elem, V](elem: Elem) extends C[Elem, V](elem)

object Test {
  val c = new C[String, String]("A") {
    override def toVal = elem
  }
  val x: c.Elem = c.elem

  val c2: C { type Elem = String } = c

  val c3 = new C[Elem = String, Value = Int]("B")
  val c4 = new C[Elem = String]("C")
  val x2: c2.Elem = c2.elem

  def d1[E, V](x: E) = new D[E, V](x)
  def d2[E, V](x: E) = new C[Elem = E, Value = V](x)

  val y1 = d1[Int, String](1)
  val y2 = d1[E = Int](2)
  val y3 = d1[V = String](3)
  val z1 = d2[E = Int, V = String](1)
  val z2 = d2[V = String, E = Int](1)
  val z3 = d2[E = Int](1)
  val z4 = d2[V = Int]("AAA")
  val z5 = d2[E = Int][V = String](1)

// Testing type inference

  def f[X <: C](x: X[Int, Int]): X[String, String] = ???
  val arg1: C[Int, Int] = ???
  val res1 = f(arg1)
  val chk1: C[String, String] = res1

  class C1[type Elem, type Value](x: Elem) extends C[Elem, Value](x)
  class CC extends C1[Int, Int](1)
  val arg2: CC = ???
  val res2 = f(arg2)
  val chk2: C[String, String] = res2

  class D1[type Elem, type Value](x: Elem) extends C[Elem, Value](x)
  class DD extends D1[Int, Int](2)
  val arg3: CC & DD = ???
  val res3 = f(arg3)
  val chk3: (C1 & D1) { type Elem = String; type Value = String } = res3
  val arg4: CC | DD = ???
  val res4 = f(arg4)
  val chk4: C[String, String] = ???

  class CX[type Elem](x: Elem) extends C1[Elem, Int](x)
  class DX[type Value]() extends D1[Int, Value](2)
  val arg5: CX[Int] & DX[Int] = ???
  val res5 = f(arg5)
  val chk5: (C1 & D1) { type Elem = String; type Value = String } = res5
  val chk6: C1[String, String] & D1[String, String] = chk5
  val chk7: (C1 & D1) { type Elem = String; type Value = String } = chk6
}

// Adapted from i94-nada, somewhat non-sensical
trait Test1 {
  trait Monad[type Elem] {
    def unit: Elem
  }
  sealed abstract class Either[A,B]
  case class Left[A,B](unit: A) extends Either[A,B] with Monad[A]
  case class Right[A,B](unit: B) extends Either[A,B] with Monad[B]
  def flatMap[X,Y,M <: Monad](m: M[Elem = X], f: X => M[Elem = Y]): M[Elem = Y] = f(m.unit)
  val res = flatMap(Left(1), {x: Int => Left(x)})
  val chk: Either[Int, Nothing] & Monad & Product1[Int] = res
}

// Adapted from i94-nada, this time with more sense
trait Test2 {
  trait Monad[type Elem] {
    def unit: Elem
  }
  sealed abstract class Either[A,B]
  case class Left[type Elem, B](unit: Elem) extends Either[Elem,B] with Monad[Elem]
  case class Right[A, type Elem](unit: Elem) extends Either[A,Elem] with Monad[Elem]
  def flatMap[X,Y,M <: Monad](m: M[Elem = X], f: X => M[Elem = Y]): M[Elem = Y] = f(m.unit)
  val res = flatMap(Left(1), {x: Int => Left(x)})
  val chk: Left[Int, Nothing] = res
}


