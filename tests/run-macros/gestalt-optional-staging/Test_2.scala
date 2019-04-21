// Port of https://github.com/liufengyun/gestalt/blob/master/macros/src/test/scala/gestalt/macros/OptionalTest.scala

object Test {

  class C

  def main(args: Array[String]): Unit = {
    getOrElseTest()
    mapTest()
    simpleGetOrElseTest()
    `don't duplicate side-effects of the prefix test`()
     hygieneTest()
    `owner chain corruptionTest`()
    `typed/untyped mixup test`()
    `the final thingTest`()
  }

  def getOrElseTest(): Unit = {
    val opt = new Optional[String]("hello")
    assert(opt.getOrElse("world") == "hello")

    val opt2 = new Optional[String](null)
    assert(opt2.getOrElse("hello") == "hello")
  }

  def mapTest(): Unit = {
    val opt = new Optional[String]("hello")
    assert(opt.map(_ + " world") == new Optional("hello world"))

    val opt2 = new Optional[String](null)
    assert(opt2.map(_ + " world") == new Optional(null))
  }

  def simpleGetOrElseTest(): Unit = {
    val c1 = new C
    val c2 = new C
    val c3 = new C
    var sideEffect = 0

    val x = new Optional(c1)
    val x1 = x.getOrElse(c2)
    assert(x1 == c1)
    assert(sideEffect == 0)

    val y = new Optional(null)
    val y1 = y.getOrElse({ sideEffect += 1; c3 })
    assert(y1 == c3)
    assert(sideEffect == 1)
  }

  def `don't duplicate side-effects of the prefix test`(): Unit = {
    val c1 = new C
    val c2 = new C
    var sideEffect = 0

    def x = { sideEffect += 1; new Optional(c1) }
    val x1 = x.getOrElse(c2)
    assert(sideEffect == 1)
  }

  def hygieneTest(): Unit = {
    val temp = 100
    new Optional(if (temp < 100) new C else null).getOrElse(new C)
  }

  def `owner chain corruptionTest`(): Unit = {
    def foo(x: => Optional[C]) = x
    foo({ val y = new Optional(null); y }).getOrElse(new C)
  }

  def `typed/untyped mixup test`(): Unit = {
    val x1 = new Optional(new C)
    val x2 = x1.map(_.toString)
  }

  def `the final thingTest`(): Unit = {
    def foo(f: => C): C = f
    val x1 = new Optional(new C)
    val x2 = x1.map(x => foo({ val y = x; y }))
  }
}
