// scalajs: --skip

object UnitAlias:
  type U = Unit

class Foo[W <: UnitAlias.U] {
  import UnitAlias.*

  type M <: U

  def bar(): U = ()
  def foobar(x: U): U = ()

  def generic[T <: U](): T = ???
  def classTypeParam(): W = ???
  def typeMember(): M = ???
}

object Test {
  def main(args: Array[String]): UnitAlias.U =
    val ms = classOf[Foo[Unit]].getDeclaredMethods().sortBy(_.getName())
    for m <- ms do
      println(m)
}
