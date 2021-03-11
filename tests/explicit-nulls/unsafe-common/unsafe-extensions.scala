class Extensions {

  extension (s1: String) def ext1(s2: String): Unit = ???
  extension (s1: String | Null) def ext2(s2: String | Null): Unit = ???

  val x: String = ???
  val y: String | Null = ???

  locally {
    x.ext1(x)
    x.ext1(y) // error
    y.ext1(x) // error
    y.ext1(y) // error
  }

  locally {
    x.ext2(x)
    x.ext2(y)
    y.ext2(x)
    y.ext2(y)
  }

  extension (ss1: Array[String]) def exts1(ss2: Array[String]): Unit = ???
  extension (ss1: Array[String | Null]) def exts2(ss2: Array[String | Null]): Unit = ???

  val xs: Array[String] = ???
  val ys: Array[String | Null] = ???

  locally {
    xs.exts1(xs)
    xs.exts1(ys) // error
    ys.exts1(xs) // error
    ys.exts1(ys) // error
  }

  locally {
    xs.exts2(xs) // error
    xs.exts2(ys) // error
    ys.exts2(xs) // error
    ys.exts2(ys)
  }

  // i7828
  locally {
    val x = "hello, world!".split(" ").map(_.length) // error
  }
}