
import scala.quoted.*

trait Ring[T] {
  def zero: T
  val one: T
  def add: (x: T, y: T) => T
  def sub: (x: T, y: T) => T
  def mul: (x: T, y: T) => T

  implicit class Ops(x: T) {
    def +(y: T): T = add(x, y)
    def -(y: T): T = sub(x, y)
    def *(y: T): T = mul(x, y)
  }
}

object RingInt extends Ring[Int] {
  val zero = 0
  val one  = 0
  val add  = (x, y) => x + y
  val sub  = (x, y) => x - y
  val mul  = (x, y) => x * y
  override def toString(): String = "RingInt"
}

class RingIntExpr(using Quotes) extends Ring[Expr[Int]] {
  val zero = '{0}
  val one  = '{1}
  val add  = (x, y) => '{$x + $y}
  val sub  = (x, y) => '{$x - $y}
  val mul  = (x, y) => '{$x * $y}
  override def toString(): String = "RingIntExpr"
}

case class RingComplex[U](u: Ring[U]) extends Ring[Complex[U]] {
  import u.*
  val zero = Complex(u.zero, u.zero)
  val one  = Complex(u.one, u.zero)
  val add = (x, y) => Complex(x.re + y.re, x.im + y.im)
  val sub = (x, y) => Complex(x.re + y.re, x.im + y.im)
  val mul = (x, y) => Complex(x.re * y.re - x.im * y.im, x.re * y.im + x.im * y.re)
  override def toString(): String = s"RingComplex($u)"
}

case class RingPV[U: ToExpr](staRing: Ring[U], dynRing: Ring[Expr[U]])(using Quotes) extends Ring[PV[U]] {
  type T = PV[U]

  val dyn = Dyns.dyn[U]
  import staRing.*
  import dynRing.*

  val zero: T = Sta(staRing.zero)
  val one: T = Sta(staRing.one)
  def add = (x: T, y: T) => (x, y) match {
    case (Sta(x), Sta(y)) => Sta(x + y)
    case (x, y) => Dyn(dyn(x) + dyn(y))
  }
  def sub = (x: T, y: T) => (x, y) match {
    case (Sta(x), Sta(y)) => Sta(x - y)
    case (x, y) => Dyn(dyn(x) - dyn(y))
  }
  def mul = (x: T, y: T) => (x, y) match {
    case (Sta(x), Sta(y)) => Sta(x * y)
    case (x, y) => Dyn(dyn(x) * dyn(y))
  }
}

class RingIntPExpr(using Quotes) extends RingPV(RingInt, new RingIntExpr)

class RingIntOPExpr(using Quotes) extends RingIntPExpr {
  override def add = (x: PV[Int], y: PV[Int]) => (x, y) match {
    case (Sta(0), y) => y
    case (x, Sta(0)) => x
    case (x, y) => super.add(x, y)
  }
  override def sub = (x: T, y: T) => (x, y) match {
    case (Sta(0), y) => y
    case (x, Sta(0)) => x
    case (x, y) => super.sub(x, y)
  }
  override def mul = (x: T, y: T) => (x, y) match {
    case (Sta(0), y) => Sta(0)
    case (x, Sta(0)) => Sta(0)
    case (Sta(1), y) => y
    case (x, Sta(1)) => x
    case (x, y) => super.mul(x, y)
  }
}
