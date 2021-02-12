object Foo {
  def bar(i: Int) = i

  def ol(i: Int) = i
  def ol(i: String) = i
}
object Test {
  import Foo.{ bar as quux, toString as bar, ol as olRenamed}

  val f1 = quux _
  val f1Typed: (Int => Int) = f1

  val f2: String => String = olRenamed _
}
