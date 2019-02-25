
object Test {

  def main(args: Array[String]): Unit = {
    val bar = new Bar
    println(fooString(bar.Baz)) // error
  }

  inline def fooString(inline x: Any): String = ${ Macros.impl(x) }

}
