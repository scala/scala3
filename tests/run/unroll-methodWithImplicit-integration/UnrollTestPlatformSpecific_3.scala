//> using options -experimental
//> using target.platform jvm
package unroll

object UnrollTestPlatformSpecificV3{
  def apply() = {
    val instance = new Unrolled()
    val cls = classOf[Unrolled]

    assert(
      cls.getMethod("foo", classOf[String], classOf[String => String]).invoke(instance, "hello", identity[String](_)) ==
        "hello1true0"
    )

    assert(scala.util.Try(cls.getMethod("foo", classOf[String], classOf[Int], classOf[String => String])).isFailure)

    assert(
      cls.getMethod("foo", classOf[String], classOf[Int], classOf[Boolean], classOf[String => String])
        .invoke(instance, "hello", 2: Integer, java.lang.Boolean.FALSE, identity[String](_)) ==
        "hello2false0"
    )
    assert(
      cls.getMethod("foo", classOf[String], classOf[Int], classOf[Boolean], classOf[Long], classOf[String => String])
        .invoke(instance, "hello", 2: Integer, java.lang.Boolean.FALSE, 3: Integer, identity[String](_)) ==
        "hello2false3"
    )

    cls.getMethods.filter(_.getName.contains("foo")).map(_.toString).sorted.foreach(println)

  }
}
