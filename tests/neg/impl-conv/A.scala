package implConv

object A {

  implicit def s2i(x: String): Int = Integer.parseInt(x) // warn: feature
  given i2s: Conversion[Int, String] = _.toString // ok

  implicit class Foo(x: String) {
    def foo = x.length
  }

}
