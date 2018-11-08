val x = 22

val y = x * 2
def square(x: Double) = x * x
square(33)

class Bar
type L = scala.List

def zip[T, U](xs: List[T], ys: List[U]) = {
  val (ps, _) = xs.foldLeft((Nil: List[(T, U)], ys)) {
    (acc, x) =>
      val (ps, ys) = acc
      ys match {
        case Nil => acc
        case y :: ys => ((x, y) :: ps, ys)
      }
    }
    ps.reverse
  }
zip(List(1, 2, 3, 4), List("a", "b", "c"))

def zips(xss: List[List[Int]]): List[List[Int]] = {
  if (xss.forall(_.nonEmpty))
    xss.map(_.head) :: zips(xss.map(_.tail))
  else Nil
}

zips(
  List(
    List(1, 2, 3),
    List(11, 22),
    List(111, 222, 333, 444)
    ))

abstract class A {
  def foo: Any
}
abstract class B extends A {
  def foo: Int
}
abstract class C extends A {
  def foo: Int
}
def f: A | B = ???
def g = f.foo


val xx = 22

trait T {
  def apply(x: Int): Unit = ()
}
class CC extends T {
  override def apply(x: Int) = super.apply(1)
}
object o {
  def apply(): String = ""
}
val s: String = o()

Double.NaN.equals(Double.NaN)

trait Status
case object A extends Status
case object B extends Status

if (true) A else B

case class Wrapper(i: Int)

trait Functor[F[_]] {
  def map[A, B](x: F[A], f: A => B): F[B]
}

class Deriver[F[_]] {
  def foldLeft[A, B](result: A, value: B): A
}

trait Eq[A] {
  def equals(x: A, y: A): Boolean
}

trait DC { type TT }

def m(x: DC): x.TT = ???
val meta = m