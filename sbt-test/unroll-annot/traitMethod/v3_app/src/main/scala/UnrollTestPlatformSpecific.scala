package unroll

object UnrollTestPlatformSpecificV3{
  def apply() = {
    val instance = new Unrolled {}
    val cls = classOf[Unrolled]

    assert(scala.util.Try(cls.getMethod("foo", classOf[String])).isFailure)
    println()
    assert(
      cls.getMethod("foo", classOf[String], classOf[Int]).invoke(instance, "hello", 2: Integer) ==
        "hello2true0"
    )
    assert(
      cls.getMethod("foo", classOf[String], classOf[Int], classOf[Boolean])
        .invoke(instance, "hello", 2: Integer, java.lang.Boolean.FALSE) ==
        "hello2false0"
    )
    assert(
      cls.getMethod("foo", classOf[String], classOf[Int], classOf[Boolean], classOf[Long])
        .invoke(instance, "hello", 2: Integer, java.lang.Boolean.FALSE, 3: Integer) ==
        "hello2false3"
    )

    cls.getMethods.filter(_.getName.contains("foo")).foreach(println)

  }
}