object Test extends dotty.runtime.LegacyApp {

  class Foo {
    def foo[@specialized U](u: U) = u
  }
  class Bar {
    def bar[@specialized U, V](u: U, v: V) = v
  }
  class Baz {
    def baz[@specialized(Int, Char) V](v: V): V = v
  }

  override def main(args: Array[String]): Unit = {
    /**
     * Expected output is:
     *
     * 10
     * 82
     * 3
     * int
     * double,int
     * int,double
     */

    val a = new Foo
    val b = new Bar
    val c = new Baz
    val foo_methods = a.getClass.getMethods
    val bar_methods = b.getClass.getMethods
    val baz_methods = c.getClass.getMethods
    println(foo_methods.filter(_.toString.contains("foo")).length)
    println(bar_methods.filter(_.toString.contains("bar")).length)
    println(baz_methods.filter(_.toString.contains("baz")).length)

    val baz_int_param = baz_methods.filter(_.toString.contains("$mcI$sp")).head.getParameterTypes.mkString(",")
    val bar_int_double_params = bar_methods.filter(s => s.toString.contains("$mcDI$sp"))
    println(baz_int_param)
    println(bar_int_double_params.head.getParameterTypes.mkString(","))
  }
}