import scala.deriving.Mirror
import scala.compiletime._

trait Schema[T] {
  def build: T
}

object Schema extends SchemaDerivation {
  implicit lazy val int: Schema[Int]   = ???
  implicit def option[A](implicit ev: Schema[A]): Schema[Option[A]] = ???
}

trait SchemaDerivation {
  inline def recurse[A <: Tuple]: List[Schema[Any]] =
    inline erasedValue[A] match {
      case _: (t *: ts) =>
        val builder = summonInline[Schema[t]].asInstanceOf[Schema[Any]]
        builder :: recurse[ts]
      case _: EmptyTuple => Nil
    }

  inline def derived[A]: Schema[A] =
    inline summonInline[Mirror.Of[A]] match {
      case m: Mirror.SumOf[A] =>
        lazy val subTypes = recurse[m.MirroredElemTypes]
        ???

      case m: Mirror.ProductOf[A] =>
        lazy val fields = recurse[m.MirroredElemTypes]
        ???
    }

  inline given gen[A]: Schema[A] = derived
}

case class X15(i: Int)
case class X14(i: X15)
case class X13(i: X14)
case class X12(i: X13)
case class X11(i: X12)
case class X10(i: X11)
case class X9(i: X10)
case class X8(i: X9)
case class X7(i: X8)
case class X6(i: X7)
case class X5(i: X6)
case class X4(i: X5)
case class X3(i: X4)
case class X2(i: X3)
case class X1(i: X2)
case class H(i: X1)
case class G(h: H)
case class F(g: G)
case class E(f: Option[F])
case class D(e: E)
case class C(d: D)
case class B(c: C)
case class A(a: A, b: B)

object TestApp {
   implicit def typeSchema: Schema[A] = Schema.gen // error // error
}
