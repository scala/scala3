//> using options -experimental
//> using target.platform jvm
package unroll

object UnrollTestPlatformSpecificV3{
  def apply() = {
    val instance = new Unrolled()
    val cls = classOf[Unrolled]

    assert(
      cls.getMethod("foo", classOf[Object]).invoke(instance, "hello") ==
        "hello1true0"
    )

    assert(scala.util.Try(cls.getMethod("foo", classOf[Object], classOf[Int])).isFailure)
    assert(
      cls.getMethod("foo", classOf[Object], classOf[Int], classOf[Boolean])
        .invoke(instance, "hello", 2: Integer, java.lang.Boolean.FALSE) ==
        "hello2false0"
    )
    assert(
      cls.getMethod("foo", classOf[Object], classOf[Int], classOf[Boolean], classOf[Long])
        .invoke(instance, "hello", 2: Integer, java.lang.Boolean.FALSE, 3: Integer) ==
        "hello2false3"
    )
    cls.getMethods.filter(_.getName.contains("foo")).map(_.toString).sorted.foreach(println)

  }
}
