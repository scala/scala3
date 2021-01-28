import scala.language.dynamics

class Foo extends scala.Dynamic {
  def selectDynamic(name: String): String = "selectDynamic(" + name + ")"
  def applyDynamic(name: String)(args: Any*): String = "applyDynamic(" + name + ")" + args.mkString("(", ", ", ")")
  def applyDynamicNamed(name: String)(args: (String, Any)*): String = "applyDynamicNamed(" + name + ")" + args.mkString("(", ", ", ")")
  def updateDynamic(name: String)(value: Any): String = "updateDynamic(" + name + ")(" + value + ")"
}

class Bar(self: String) extends scala.Dynamic {
  def selectDynamic(name: String): Bar = new Bar(self + ".selectDynamic(" + name + ")")
  def applyDynamic(name: String)(args: Any*): Bar = new Bar(self + ".applyDynamic(" + name + ")" + args.mkString("(", ", ", ")"))
  def applyDynamicNamed(name: String)(args: (String, Any)*): Bar = new Bar(self + ".applyDynamicNamed(" + name + ")" + args.mkString("(", ", ", ")"))
  def updateDynamic(name: String)(value: Any): Bar = new Bar(self + ".updateDynamic(" + name + ")(" + value + ")")
  def update(key: Int, value: Int): Bar = new Bar(self + ".update(" + key + ", " + value + ")")
  override def toString = self
}

class Baz extends scala.Dynamic {
  def selectDynamic(name: String): String = "selectDynamic(" + name + ")"
  def applyDynamic(name: String)(args: String*): String = "applyDynamic(" + name + ")" + args.mkString("(", ", ", ")")
  def applyDynamicNamed(name: String)(args: (String, Any)*): String = "applyDynamicNamed(" + name + ")" + args.mkString("(", ", ", ")")
  def updateDynamic(name: String)(value: String): String = "updateDynamic(" + name + ")(" + value + ")"
}

class Qux extends scala.Dynamic {
  def selectDynamic[T](name: String): String = "selectDynamic(" + name + ")"
  def applyDynamic[T](name: String)(args: String*): String = "applyDynamic(" + name + ")" + args.mkString("(", ", ", ")")
  def applyDynamicNamed[T](name: String)(args: (String, Any)*): String = "applyDynamicNamed(" + name + ")" + args.mkString("(", ", ", ")")
  def updateDynamic[T](name: String)(value: T): String = "updateDynamic(" + name + ")(" + value + ")"
}

object Test {
  val qux = new Qux

  implicit class StringUpdater(str: String) {
    def update(name: String, v: String) = s"$str.update(" + name + ", " + v + ")"
  }

  var failed = false

  def assertEquals(expected: String, actual: Any): Unit = {
    if (expected != actual.toString) {
      println("Error: expected <" + expected + "> but was <" + actual.toString + ">")
      failed = true
    }
  }

  def main(args: Array[String]): Unit = {
    runFooTests1()
    runFooTests2()
    runFooTests3()
    runBarTests()
    runBazTests()
    runQuxTests()
    assert(!failed)
  }

  /** Test the basics of the transfomation rules. */
  def runFooTests1() = {
    val foo = new Foo

    assertEquals("selectDynamic(bazSelect)", foo.bazSelect)

    assertEquals("applyDynamic(bazApply)()", foo.bazApply())
    assertEquals("applyDynamic(bazApply)(1)", foo.bazApply(1))
    assertEquals("applyDynamic(bazApply)(1, 2, 3)", foo.bazApply(1, 2, 3))
    assertEquals("applyDynamic(bazApply)(1, 2, a)", foo.bazApply(1, 2, "a"))
    assertEquals("applyDynamic(bazApply)(1, 2, a)", foo.bazApply(List(1, 2, "a")*))

    assertEquals("applyDynamicNamed(bazApply)((a,1))", foo.bazApply(a = 1))
    assertEquals("applyDynamicNamed(bazApply)((a,1), (b,2))", foo.bazApply(a = 1, b = 2))
    assertEquals("applyDynamicNamed(bazApply)((a,1), (,0))", foo.bazApply(a = 1, 0))
    assertEquals("applyDynamicNamed(bazApply)((a,1), (a,5))", foo.bazApply(a = 1, a = 5))
    assertEquals("applyDynamicNamed(bazApply)((,d), (a,1), (,5), (a,c))", foo.bazApply("d", a = 1, 5, a = 'c'))

    assertEquals("updateDynamic(bazUpdate)(abc)", foo.bazUpdate = "abc")

    assertEquals("selectDynamic(bazSelectUpdate).update(key, value)", foo.bazSelectUpdate("key") = "value")
  }

  /** Test implicit conversions kick in before dynamic calls. */
  def runFooTests2() = {
    implicit class Bar(foo: Foo) {
      def bazSelect: String = "Bar.bazSelect"
      def bazApply(args: Any*): String = args.mkString("Bar.bazApply(", ", ", ")")
      def bazApplyNamed(a: Int = -1, b: String = "-1"): String = "Bar.bazApplyNamed(" + a + ", " + b + ")"
    }

    val foo = new Foo

    assertEquals("Bar.bazSelect", foo.bazSelect)

    assertEquals("Bar.bazApply()", foo.bazApply())
    assertEquals("Bar.bazApply(1)", foo.bazApply(1))
    assertEquals("Bar.bazApply(1, 2, 3)", foo.bazApply(1, 2, 3))
    assertEquals("Bar.bazApply(1, 2, a)", foo.bazApply(1, 2, "a"))

    assertEquals("Bar.bazApplyNamed(1, -1)", foo.bazApplyNamed(a = 1))
    assertEquals("Bar.bazApplyNamed(1, 2)", foo.bazApplyNamed(a = 1, b = "2"))
    assertEquals("Bar.bazApplyNamed(1, abc)", foo.bazApplyNamed(a = 1, "abc"))

    assertEquals("selectDynamic(bazSelectUpdate).update(key, value)", foo.bazSelectUpdate("key") = "value")
  }

  /** Test apply insertions kick in before dynamic calls. */
  def runFooTests3() = {
    val foo = new Foo

    assertEquals("applyDynamic(apply)()", foo())
    assertEquals("applyDynamic(apply)(1)", foo(1))
    assertEquals("applyDynamic(apply)(1, 2, 3)", foo(1, 2, 3))
    assertEquals("applyDynamic(apply)(1, 2, a)", foo(1, 2, "a"))
    assertEquals("applyDynamic(apply)(1, 2, a)", foo(List(1, 2, "a")*))

    assertEquals("applyDynamicNamed(apply)((a,1))", foo(a = 1))
    assertEquals("applyDynamicNamed(apply)((a,1), (b,2))", foo(a = 1, b = 2))
    assertEquals("applyDynamicNamed(apply)((a,1), (,0))", foo(a = 1, 0))
    assertEquals("applyDynamicNamed(apply)((a,1), (a,5))", foo(a = 1, a = 5))
    assertEquals("applyDynamicNamed(apply)((,d), (a,1), (,5), (a,c))", foo("d", a = 1, 5, a = 'c'))

    assertEquals("applyDynamic(apply)()", foo.apply())
    assertEquals("applyDynamic(apply)(1)", foo.apply(1))
    assertEquals("applyDynamic(apply)(1, 2, 3)", foo.apply(1, 2, 3))
    assertEquals("applyDynamic(apply)(1, 2, a)", foo.apply(1, 2, "a"))
    assertEquals("applyDynamic(apply)(1, 2, a)", foo.apply(List(1, 2, "a")*))

    assertEquals("applyDynamicNamed(apply)((a,1))", foo.apply(a = 1))
    assertEquals("applyDynamicNamed(apply)((a,1), (b,2))", foo.apply(a = 1, b = 2))
    assertEquals("applyDynamicNamed(apply)((a,1), (,0))", foo.apply(a = 1, 0))
    assertEquals("applyDynamicNamed(apply)((a,1), (a,5))", foo.apply(a = 1, a = 5))
    assertEquals("applyDynamicNamed(apply)((,d), (a,1), (,5), (a,c))", foo.apply("d", a = 1, 5, a = 'c'))

    object bar {
      val foo: Foo = new Foo
    }

    assertEquals("applyDynamic(apply)()", bar.foo())
    assertEquals("applyDynamic(apply)(1)", bar.foo(1))
    assertEquals("applyDynamic(apply)(1, 2, 3)", bar.foo(1, 2, 3))
    assertEquals("applyDynamic(apply)(1, 2, a)", bar.foo(1, 2, "a"))
    assertEquals("applyDynamic(apply)(1, 2, a)", bar.foo(List(1, 2, "a")*))

    assertEquals("applyDynamicNamed(apply)((a,1))", bar.foo(a = 1))
    assertEquals("applyDynamicNamed(apply)((a,1), (b,2))", bar.foo(a = 1, b = 2))
    assertEquals("applyDynamicNamed(apply)((a,1), (,0))", bar.foo(a = 1, 0))
    assertEquals("applyDynamicNamed(apply)((a,1), (a,5))", bar.foo(a = 1, a = 5))
    assertEquals("applyDynamicNamed(apply)((,d), (a,1), (,5), (a,c))", bar.foo("d", a = 1, 5, a = 'c'))

    assertEquals("applyDynamic(apply)()", bar.foo.apply())
    assertEquals("applyDynamic(apply)(1)", bar.foo.apply(1))
    assertEquals("applyDynamic(apply)(1, 2, 3)", bar.foo.apply(1, 2, 3))
    assertEquals("applyDynamic(apply)(1, 2, a)", bar.foo.apply(1, 2, "a"))
    assertEquals("applyDynamic(apply)(1, 2, a)", bar.foo.apply(List(1, 2, "a")*))

    assertEquals("applyDynamicNamed(apply)((a,1))", bar.foo.apply(a = 1))
    assertEquals("applyDynamicNamed(apply)((a,1), (b,2))", bar.foo.apply(a = 1, b = 2))
    assertEquals("applyDynamicNamed(apply)((a,1), (,0))", bar.foo.apply(a = 1, 0))
    assertEquals("applyDynamicNamed(apply)((a,1), (a,5))", bar.foo.apply(a = 1, a = 5))
    assertEquals("applyDynamicNamed(apply)((,d), (a,1), (,5), (a,c))", bar.foo.apply("d", a = 1, 5, a = 'c'))
  }

  /** Test cains of dynamic calls. */
  def runBarTests() = {
    val bar = new Bar("bar")

    // dynamics combined with themselves
    assertEquals("bar.selectDynamic(select1).selectDynamic(select2).selectDynamic(select3)",
        bar.select1.select2.select3)
    assertEquals("bar.applyDynamic(apply1)().applyDynamic(apply2)().applyDynamic(apply3)()",
        bar.apply1().apply2().apply3())
    assertEquals("bar.applyDynamic(apply1)(1).applyDynamic(apply2)(1, 2).applyDynamic(apply3)(1, 2, 3)",
        bar.apply1(1).apply2(1, 2).apply3(1, 2, 3))
    assertEquals("bar.applyDynamicNamed(apply1)((a,1)).applyDynamicNamed(apply2)((,1), (b,2))",
        bar.apply1(a = 1).apply2(1, b = 2))
    assertEquals("bar.updateDynamic(update1)(1).updateDynamic(update2)(4)", (bar.update1 = 1).update2 = 4)
    assertEquals("bar.selectDynamic(update1).update(1, 2).selectDynamic(update2).update(3, 4)",
        (bar.update1(1) = 2).update2(3) = 4)

    // selectDynamic combined with every other dynamic
    assertEquals("bar.applyDynamic(apply1)().selectDynamic(select1)", bar.apply1().select1)
    assertEquals("bar.selectDynamic(select1).applyDynamic(apply2)()", bar.select1.apply2())
    assertEquals("bar.applyDynamicNamed(apply1)((a,1)).selectDynamic(select1)", bar.apply1(a = 1).select1)
    assertEquals("bar.selectDynamic(select1).applyDynamicNamed(apply1)((a,1))", bar.select1.apply1(a = 1))
    assertEquals("bar.updateDynamic(update1)(1).selectDynamic(select1)", (bar.update1 = 1).select1)
    assertEquals("bar.selectDynamic(select1).updateDynamic(update1)(1)", bar.select1.update1 = 1)
    assertEquals("bar.selectDynamic(update1).update(0, 1).selectDynamic(select1)", (bar.update1(0) = 1).select1)
    assertEquals("bar.selectDynamic(select1).selectDynamic(update1).update(0, 1)", bar.select1.update1(0) = 1)

    // applyDynamic combined with every remaninig dynamic
    assertEquals("bar.applyDynamic(apply1)().applyDynamicNamed(apply2)((a,1))", bar.apply1().apply2(a = 1))
    assertEquals("bar.applyDynamicNamed(apply1)((a,1)).applyDynamic(apply2)()", bar.apply1(a = 1).apply2())
    assertEquals("bar.applyDynamic(apply1)().updateDynamic(update1)(1)", bar.apply1().update1 = 1)
    assertEquals("bar.updateDynamic(update1)(1).applyDynamic(apply1)()", (bar.update1 = 1).apply1())
    assertEquals("bar.applyDynamic(apply1)().selectDynamic(update1).update(0, 1)", bar.apply1().update1(0) = 1)
    assertEquals("bar.selectDynamic(update1).update(0, 1).applyDynamic(apply1)()", (bar.update1(0) = 1).apply1())

    // applyDynamicNamed combined with every remaninig dynamic
    assertEquals("bar.applyDynamicNamed(apply1)((a,1)).updateDynamic(update1)(1)", bar.apply1(a = 1).update1 = 1)
    assertEquals("bar.updateDynamic(update1)(1).applyDynamicNamed(apply1)((a,1))", (bar.update1 = 1).apply1(a = 1))
    assertEquals("bar.applyDynamicNamed(apply1)((a,1)).selectDynamic(update1).update(0, 1)", bar.apply1(a = 1).update1(0) = 1)
    assertEquals("bar.selectDynamic(update1).update(0, 1).applyDynamicNamed(apply1)((a,1))", (bar.update1(0) = 1).apply1(a = 1))

    // updateDynamic combined selectDynamic(_).update(_, _)
    assertEquals("bar.updateDynamic(update1)(1).selectDynamic(update2).update(0, 1)", (bar.update1 = 1).update2(0) = 1)
    assertEquals("bar.selectDynamic(update1).update(0, 1).updateDynamic(update2)(1)", (bar.update1(0) = 1).update2 = 1)
  }

  /** Test implicit conversion in the arguments of the dynamic call. */
  def runBazTests() = {
    implicit def intToString(n: Int): String = n.toString

    val baz = new Baz

    assertEquals("applyDynamic(bazApply)()", baz.bazApply())
    assertEquals("applyDynamic(bazApply)(1)", baz.bazApply(1))
    assertEquals("applyDynamic(bazApply)(1, 2, 3)", baz.bazApply(1, 2, 3))
    assertEquals("applyDynamic(bazApply)(1, 2, a)", baz.bazApply(1, 2, "a"))

    assertEquals("applyDynamicNamed(bazApply)((a,1))", baz.bazApply(a = 1))
    assertEquals("applyDynamicNamed(bazApply)((a,1), (b,2))", baz.bazApply(a = 1, b = 2))
    assertEquals("applyDynamicNamed(bazApply)((a,1), (,0))", baz.bazApply(a = 1, 0))
    assertEquals("applyDynamicNamed(bazApply)((a,1), (a,5))", baz.bazApply(a = 1, a = 5))
    assertEquals("applyDynamicNamed(bazApply)((,4), (a,1), (,5), (a,9))", baz.bazApply(4, a = 1, 5, a = 9))

    assertEquals("updateDynamic(bazUpdate)(10)", baz.bazUpdate = 10)

    assertEquals("selectDynamic(bazSelectUpdate).update(key, 10)", baz.bazSelectUpdate("key") = 10)
    assertEquals("selectDynamic(bazSelectUpdate).update(7, value)", baz.bazSelectUpdate(7) = "value")
    assertEquals("selectDynamic(bazSelectUpdate).update(7, 10)", baz.bazSelectUpdate(7) = 10)
  }

  /** Test correct lifting of type parameters */
  def runQuxTests() = {
    implicit def intToString(n: Int): String = n.toString

    val qux = new Qux

    assertEquals("selectDynamic(quxSelect)", qux.quxSelect)
    assertEquals("selectDynamic(quxSelect)", qux.quxSelect[Int])

    assertEquals("applyDynamic(quxApply)()", qux.quxApply())
    assertEquals("applyDynamic(quxApply)()", qux.quxApply[Int]())
    assertEquals("applyDynamic(quxApply)(1)", qux.quxApply(1))
    assertEquals("applyDynamic(quxApply)(1)", qux.quxApply[Int](1))
    assertEquals("applyDynamic(quxApply)(1, 2, 3)", qux.quxApply(1, 2, 3))
    assertEquals("applyDynamic(quxApply)(1, 2, 3)", qux.quxApply[Int](1, 2, 3))
    assertEquals("applyDynamic(quxApply)(1, 2, a)", qux.quxApply(1, 2, "a"))
    assertEquals("applyDynamic(quxApply)(1, 2, a)", qux.quxApply[Int](1, 2, "a"))

    assertEquals("applyDynamicNamed(quxApplyNamed)((a,1))", qux.quxApplyNamed(a = 1))
    assertEquals("applyDynamicNamed(quxApplyNamed)((a,1))", qux.quxApplyNamed[Int](a = 1))
    assertEquals("applyDynamicNamed(quxApplyNamed)((a,1), (b,2))", qux.quxApplyNamed(a = 1, b = "2"))
    assertEquals("applyDynamicNamed(quxApplyNamed)((a,1), (b,2))", qux.quxApplyNamed[Int](a = 1, b = "2"))
    assertEquals("applyDynamicNamed(quxApplyNamed)((a,1), (,abc))", qux.quxApplyNamed(a = 1, "abc"))
    assertEquals("applyDynamicNamed(quxApplyNamed)((a,1), (,abc))", qux.quxApplyNamed[Int](a = 1, "abc"))

    assertEquals("updateDynamic(quxUpdate)(abc)", qux.quxUpdate = "abc")

    assertEquals("selectDynamic(quxSelectUpdate).update(key, value)", qux.quxSelectUpdate("key") = "value")
    assertEquals("selectDynamic(quxSelectUpdate).update(key, value)", qux.quxSelectUpdate[Int]("key") = "value")
  }
}
