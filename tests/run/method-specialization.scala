object Test extends dotty.runtime.LegacyApp {

  class Foo {
    def foo[@specialized U](u: U) = u
  }
  class Bar {
    def bar[@specialized U,@specialized V](u: U, v: V) = v
  }
  class Baz {
    def baz[@specialized(Int, Char) V](v: V): V = v
  }
  class Kung {
    def kung[@specialized U, V](u: U, v: V) = println(u.getClass)
  }

  override def main(args: Array[String]): Unit = {
    /**
     * Expected output is:
     *
     * 10
     * 82
     * 3
     * 10
     * int
     * double,int
     * int,class java.lang.Object
     */

    val a = new Foo
    val b = new Bar
    val c = new Baz
    val d = new Kung
    val foo_methods = a.getClass.getMethods
    val bar_methods = b.getClass.getMethods
    val baz_methods = c.getClass.getMethods
    val kung_methods = d.getClass.getMethods
    println(foo_methods.filter(_.toString.contains("foo")).length)
    println(bar_methods.filter(_.toString.contains("bar")).length)
    println(baz_methods.filter(_.toString.contains("baz")).length)
    println(kung_methods.filter(_.toString.contains("kung")).length)

    val baz_int_param = baz_methods.filter(_.toString.contains("$mIc$sp")).head.getParameterTypes.mkString(",")
    val bar_int_double_params = bar_methods.filter(s => s.toString.contains("$mDIc$sp"))
    val kung_int_gen_params = kung_methods.filter(s => s.toString.contains("$mIc$sp"))
    println(baz_int_param)
    println(bar_int_double_params.head.getParameterTypes.mkString(","))
    println(kung_int_gen_params.head.getParameterTypes.mkString(","))

    def genericKung[A](a: A) = d.kung(a, a)
    genericKung(1)

    d.kung(1, 1)
    d.kung(1.0, 1.0)
  }
}