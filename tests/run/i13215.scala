import scala.annotation.binaryAPI

package foo {
  trait Bar:
    inline def baz = Baz

  @binaryAPI private[foo] object Baz
}

@main def Test: Unit =
  val bar = new foo.Bar {}
  bar.baz
