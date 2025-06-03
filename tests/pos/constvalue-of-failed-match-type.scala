import scala.compiletime.*

object Inlines:
  inline def testInline[A](): Boolean =
    inline erasedValue[A] match
      case _: Tuple =>
        constValue[Tuple.Size[A & Tuple]] == 2
      case _ =>
        false
end Inlines

case class Foo2(x: Boolean, y: String)
case class Foo3(x: Boolean, y: String, z: Int)

object Test:
  def main(args: Array[String]): Unit =
    // Note: the assert's don't do anything since it's a pos test; they show intent (and pass if we run the test)
    assert(!Inlines.testInline[Foo2]())
    assert(!Inlines.testInline[Foo3]())
    assert(Inlines.testInline[(Boolean, String)]())
    assert(!Inlines.testInline[(Boolean, String, Int)]())
end Test
