class S {
  class O {
    def f(s: String | Null): String | Null = ???
    def f(ss: Array[String] | Null): Array[String] | Null = ???

    def g(s: String): String = ???
    def g(ss: Array[String]): Array[String] = ???

    def h(ts: String => String): String = ???
    def h(ts: Array[String] => Array[String]): Array[String] = ???

    def i(ts: String | Null => String | Null): String | Null = ???
    def i(ts: Array[String] | Null => Array[String] | Null): Array[String] | Null = ???
  }

  import scala.language.unsafeNulls

  val o: O = ???

  locally {
    def h1(hh: String => String) = ???
    def h2(hh: Array[String] => Array[String]) = ???
    def f1(x: String | Null): String | Null = ???
    def f2(x: Array[String | Null]):  Array[String | Null] = ???

    h1(f1)
    h1(o.f)

    h2(f2)
    h2(o.f)
  }

  locally {
    def h1(hh: String | Null => String | Null) = ???
    def h2(hh: Array[String | Null] => Array[String | Null]) = ???
    def g1(x: String): String = ???
    def g2(x: Array[String]):  Array[String] = ???

    h1(g1)
    h1(o.g)

    h2(g2)
    h2(o.g)
  }

  locally {
    def f1(x: String | Null): String | Null = ???
    def f2(x: Array[String | Null]):  Array[String | Null] = ???

    o.h(f1)
    o.h(f2)
  }

  locally {
    def g1(x: String): String = ???
    def g2(x: Array[String]):  Array[String] = ???

    o.i(g1)
    o.i(g2)
  }
}