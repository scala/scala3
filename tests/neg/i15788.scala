
import scala.deriving.Mirror
import scala.compiletime.*

trait Test[A] {
  def test: String
}

object Test {

  inline def findA[T <: Tuple]: Unit =
    inline erasedValue[T] match {
      case _: EmptyTuple  => error("Field 'a' not found")
      case _: ("a" *: tl) => ()
      case _: (_ *: tl)   => findA[tl]
    }

  inline given [A <: Product] => (mm: Mirror.ProductOf[A]) => Test[A] = new {
    override def test: String = {
      findA[mm.MirroredElemLabels]
      "test"
    }
  }
}

final case class Test1(a: String, b: Int)
final case class Test2(q: String, w: Int)

object Main {
  inline def fff1[P <: Product](using ggg: Test[P]): String = {
    ggg.test
  }

  inline def fff2[P <: Product]: String = {
    summonInline[Test[P]].test
  }

  fff1[Test1]
  fff1[Test2] // error

  summonInline[Test[Test1]].test
  summonInline[Test[Test2]].test // error

  fff2[Test1]
  fff2[Test2] // error
}
