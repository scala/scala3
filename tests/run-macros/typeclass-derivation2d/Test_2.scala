import scala.collection.mutable

import Deriving._

sealed trait Lst[+T] // derives Eq, Pickler, Show

object Lst extends Mirror.Sum {
  type _MonoType = Lst[_]

  def ordinal(x: Lst[_]) = x match {
    case x: Cons[_] => 0
    case Nil => 1
  }

  implicit def mirror[T]: Mirror.Sum {
    type _MonoType = Lst[T]
    type ElemTypes = (Cons[T], Nil.type)
  } = this.asInstanceOf

  case class Cons[T](hd: T, tl: Lst[T]) extends Lst[T]

  object Cons extends Mirror.Product {
    type _MonoType = Lst[_]

    def apply[T](x: T, xs: Lst[T]): Lst[T] = new Cons(x, xs)

    def _fromProduct(p: Product): Cons[_] =
      new Cons(productElement[Any](p, 0), productElement[Lst[Any]](p, 1))

    implicit def mirror[T]: Mirror.Product {
      type _MonoType = Cons[T]
      type ElemTypes = (T, Lst[T])
      type CaseLabel = "Cons"
      type ElemLabels = ("hd", "tl")
    } = this.asInstanceOf
  }

  case object Nil extends Lst[Nothing] with Mirror.Singleton {

    implicit def mirror: Mirror.Singleton {
      type _MonoType = Nil.type
      type ElemTypes = Unit
      type CaseLabel = "Nil"
      type ElemLabels = Unit
    } = this.asInstanceOf
  }

  // three clauses that would be generated from a `derives` clause
  implicit def derived$Eq[T: Eq]: Eq[Lst[T]] = Eq.derived
  implicit def derived$Pickler[T: Pickler]: Pickler[Lst[T]] = Pickler.derived
  implicit def derived$Show[T: Show]: Show[Lst[T]] = Show.derived
}

// --------------- A simple product type ------------------------

case class Pair[T](x: T, y: T) // derives Eq, Pickler, Show

object Pair extends Mirror.Product {
  type _MonoType = Pair[_]

  def _fromProduct(p: Product): Pair[_] =
    Pair(productElement[Any](p, 0), productElement[Any](p, 1))

  implicit def mirror[T]: Mirror.Product {
    type _MonoType = Pair[T]
    type ElemTypes = (T, T)
    type CaseLabel = "Pair"
    type ElemLabels = ("x", "y")
  } = this.asInstanceOf

  // clauses that could be generated from a `derives` clause
  implicit def derived$Eq[T: Eq]: Eq[Pair[T]] = Eq.derived
  implicit def derived$Pickler[T: Pickler]: Pickler[Pair[T]] = Pickler.derived
  implicit def derived$Show[T: Show]: Show[Pair[T]] = Show.derived
}

// ----------- Another sum type ----------------------------------------

sealed trait Either[+L, +R] extends Product with Serializable // derives Eq, Pickler, Show

object Either extends Mirror.Sum {
  type _MonoType = Either[_, _]

  def ordinal(x: Either[_, _]) = x match {
    case x: Left[_] => 0
    case x: Right[_] => 1
  }

  implicit def mirror[L, R]: Mirror.Sum {
    type _MonoType = Either[L, R]
    type ElemTypes = (Left[L], Right[R])
  } = this.asInstanceOf

  implicit def derived$Eq[L: Eq, R: Eq]: Eq[Either[L, R]] = Eq.derived
  implicit def derived$Pickler[L: Pickler, R: Pickler]: Pickler[Either[L, R]] = Pickler.derived
  implicit def derived$Show[L: Show, R: Show]: Show[Either[L, R]] = Show.derived
}

case class Left[L](elem: L) extends Either[L, Nothing]
case class Right[R](elem: R) extends Either[Nothing, R]

object Left extends Mirror.Product {
  type _MonoType = Left[_]
  def _fromProduct(p: Product): Left[_] = Left(productElement[Any](p, 0))
  implicit def mirror[L]: Mirror.Product {
    type _MonoType = Left[L]
    type ElemTypes = L *: Unit
    type CaseLabel = "Left"
    type ElemLabels = "x" *: Unit
  } = this.asInstanceOf
}

object Right extends Mirror.Product {
  type _MonoType = Right[_]
  def _fromProduct(p: Product): Right[_] = Right(productElement[Any](p, 0))
  implicit def mirror[R]: Mirror.Product {
    type _MonoType = Right[R]
    type ElemTypes = R *: Unit
    type CaseLabel = "Right"
    type ElemLabels = "x" *: Unit
  } = this.asInstanceOf
}

object Test extends App {
  val eq = implicitly[Eq[Lst[Int]]]
  val xs = Lst.Cons(11, Lst.Cons(22, Lst.Cons(33, Lst.Nil)))
  val ys = Lst.Cons(11, Lst.Cons(22, Lst.Nil))
  assert(eq.eql(xs, xs))
  assert(!eq.eql(xs, ys))
  assert(!eq.eql(ys, xs))
  assert(eq.eql(ys, ys))

  val eq2 = implicitly[Eq[Lst[Lst[Int]]]]
  val xss = Lst.Cons(xs, Lst.Cons(ys, Lst.Nil))
  val yss = Lst.Cons(xs, Lst.Nil)
  assert(eq2.eql(xss, xss))
  assert(!eq2.eql(xss, yss))
  assert(!eq2.eql(yss, xss))
  assert(eq2.eql(yss, yss))

  val buf = new mutable.ListBuffer[Int]
  val pkl = implicitly[Pickler[Lst[Int]]]
  pkl.pickle(buf, xs)
  println(buf)
  val xs1 = pkl.unpickle(buf)
  println(xs1)
  assert(xs1 == xs)
  assert(eq.eql(xs1, xs))

  val pkl2 = implicitly[Pickler[Lst[Lst[Int]]]]
  pkl2.pickle(buf, xss)
  println(buf)
  val xss1 = pkl2.unpickle(buf)
  println(xss1)
  assert(xss == xss1)
  assert(eq2.eql(xss, xss1))

  val p1 = Pair(1, 2)
  val p2 = Pair(1, 2)
  val p3 = Pair(2, 1)
  val eqp = implicitly[Eq[Pair[Int]]]
  assert(eqp.eql(p1, p2))
  assert(!eqp.eql(p2, p3))

  val pklp = implicitly[Pickler[Pair[Int]]]
  pklp.pickle(buf, p1)
  println(buf)
  val p1a = pklp.unpickle(buf)
  println(p1a)
  assert(p1 == p1a)
  assert(eqp.eql(p1, p1a))

  def showPrintln[T: Show](x: T): Unit =
    println(implicitly[Show[T]].show(x))

  showPrintln(xs)
  showPrintln(xss)

  val zs = Lst.Cons(Left(1), Lst.Cons(Right(Pair(2, 3)), Lst.Nil))
  showPrintln(zs)

  def pickle[T: Pickler](buf: mutable.ListBuffer[Int], x: T): Unit =
    implicitly[Pickler[T]].pickle(buf, x)

  def unpickle[T: Pickler](buf: mutable.ListBuffer[Int]): T =
    implicitly[Pickler[T]].unpickle(buf)

  def copy[T: Pickler](x: T): T = {
    val buf = new mutable.ListBuffer[Int]
    pickle(buf, x)
    unpickle[T](buf)
  }

  def eql[T: Eq](x: T, y: T) = implicitly[Eq[T]].eql(x, y)

  val zs1 = copy(zs)
  showPrintln(zs1)
  assert(eql(zs, zs1))
}
