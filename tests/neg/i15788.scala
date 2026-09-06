
import scala.compiletime.*
import scala.deriving.Mirror

trait Test[A]:
  def test: String

object Test:

  inline def findA[T <: Tuple]: Unit =
    inline erasedValue[T] match
    case _: EmptyTuple  => error("Field 'a' not found")
    case _: ("a" *: tl) => ()
    case _: (_ *: tl)   => findA[tl]

  inline given [A <: Product] => (mm: Mirror.ProductOf[A]) => Test[A] = new {
    override def test: String =
      findA[mm.MirroredElemLabels]
      "test"
  }
end Test

final case class Test1(a: String, b: Int)
final case class Test2(v: String, w: Int)

@main def main =
  inline def f[P <: Product](using g: Test[P]): String = g.test

  inline def g[P <: Product]: String = summonInline[Test[P]].test

  f[Test1]
  f[Test2] // error at inlining

  summonInline[Test[Test1]].test
  summonInline[Test[Test2]].test // error

  g[Test1]
  g[Test2] // error
