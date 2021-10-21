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
        new Schema[A] {
          def build: A = ???
        }

      case m: Mirror.ProductOf[A] =>
        lazy val fields = recurse[m.MirroredElemTypes]
        new Schema[A] {
          def build: A = ???
        }
    }

  inline given gen[A]: Schema[A] = derived
}

case class H(i: Int)
case class G(h: H)
case class F(g: G)
case class E(f: Option[F])
case class D(e: E)
case class C(d: D)
case class B(c: C)
case class A(a: A, b: B)

object TestApp {
   implicit def typeSchema: Schema[A] = Schema.gen
}
