
object Test {

  def main(args: Array[String]): Unit = {
    println(fooString(scala.collection.immutable.Nil))
    println(fooString(None))
    println(fooString(Bar))
    println(fooString(Bar.Baz))
    println(fooString(foo.Bar))
    println(fooString(foo.Bar.Baz))
  }

  inline def fooString(inline x: Any): String = ${Macros.impl(x)}

}
