package implConv

object A {

  implicit def s2i(x: String): Int = Integer.parseInt(x) // error: feature
  given Conversion[Int, String] as i2s = _.toString // ok

  implicit class Foo(x: String) {
    def foo = x.length
  }

}
