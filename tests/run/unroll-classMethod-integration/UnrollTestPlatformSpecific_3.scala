//> using options -experimental
//> using target.platform jvm
package unroll

object UnrollTestPlatformSpecificV3{
  def apply() = {
    val instance = new Unrolled()
    val cls = classOf[Unrolled]

    assert(
      cls.getMethod("foo", classOf[String]).invoke(instance, "hello") ==
        "hello1true0"
    )

    // Only generate unrolled methods for annotated params
    // (b: Boolean) is not annotated so this method should not exist
    assert(scala.util.Try(cls.getMethod("foo", classOf[String], classOf[Int])).isFailure)

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

    cls.getMethods.filter(_.getName.contains("foo")).map(_.toString).sorted.foreach(println)
  }
}
