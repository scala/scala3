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

  val o: O = ???

  locally {
    def h1(hh: String => String): Unit = ???
    def h2(hh: Array[String] => Array[String]): Unit = ???
    def f1(x: String | Null): String | Null = ???
    def f2(x: Array[String | Null]):  Array[String | Null] = ???

    h1(f1) // error
    h1(o.f) // error

    h2(f2) // error
    h2(o.f) // error
  }

  locally {
    def h1(hh: String | Null => String | Null): Unit = ???
    def h2(hh: Array[String | Null] => Array[String | Null]): Unit = ???
    def g1(x: String): String = ???
    def g2(x: Array[String]): Array[String] = ???

    h1(g1) // error
    h1(o.g) // error

    h2(g2) // error
    h2(o.g) // error
  }

  locally {
    def f1(x: String | Null): String | Null = ???
    def f2(x: Array[String | Null]):  Array[String | Null] = ???

    o.h(f1) // error
    o.h(f2) // error
  }

  locally {
    def g1(x: String): String = ???
    def g2(x: Array[String]): Array[String] = ???

    o.i(g1) // error
    o.i(g2) // error
  }
}