// scalajs: --skip
//Test java runtime reflection access to @Runtime annotations on method parameters.
@main def Test =
  def check(actual: Int)(expect: Int)(msg: => String): Unit =
    assert(actual == expect, s"$msg expected $expect but actually $actual")
  locally:
    // def bar[T](a: String, @Foo v: Int)(@Foo b: T, @Blah w: Int)
    val method = classOf[Bar].getMethod("bar", classOf[String], classOf[Int], classOf[Object], classOf[Int])
    val annots = method.getParameterAnnotations()
    check(annots.length)(4)("param count")
    check(annots(0).length)(0)("count at 0")
    check(annots(1).length)(1)("count at 1")
    assert(annots(1)(0).isInstanceOf[Foo], "expect Foo at 1")
    check(annots(2).length)(1)("count at 2")
    assert(annots(2)(0).isInstanceOf[Foo], "expect Foo at 2")
    check(annots(3).length)(0)("count at 3")

  locally:
    // def bar2(@Foo v: Int)
    val method = classOf[Bar].getMethod("bar2", classOf[Int])
    val annots = method.getParameterAnnotations()
    check(annots.length)(1)("param count")
    assert(annots(0)(0).isInstanceOf[Foo], "expect Foo at 0")

  locally:
    // extension [T](s: String) def f[U](@Foo t: T, @Foo u: U) = ()
    val method = classOf[Bar].getMethod("f", classOf[String], classOf[Object], classOf[Object])
    val annots = method.getParameterAnnotations()
    check(annots.length)(3)("param count")
    check(annots(0).length)(0)("count at 0")
    check(annots(1).length)(1)("count at 1")
    check(annots(2).length)(1)("count at 2")
    assert(annots(1)(0).isInstanceOf[Foo], "expect Foo at 1")
    assert(annots(2)(0).isInstanceOf[Foo], "expect Foo at 2")
