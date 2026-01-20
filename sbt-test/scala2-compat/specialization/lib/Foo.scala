import scala.collection.immutable.NumericRange

object Foo {
  def foo: Unit = {
    val range = NumericRange(0, 2, 1)
    val f: Int => Unit = i => println(i)
    range.foreach(f)
  }
}
