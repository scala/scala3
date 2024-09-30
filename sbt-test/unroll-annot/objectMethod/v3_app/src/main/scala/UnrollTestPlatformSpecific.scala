package unroll

object UnrollTestPlatformSpecificV3{
  def apply() = {
    val instance = Unrolled
    val instanceCls = Class.forName("unroll.Unrolled$")

    instanceCls.getMethods.filter(_.getName.contains("foo")).foreach(println)

    // Make sure singleton instance forwarder methods are generated
    assert(scala.util.Try(instanceCls.getMethod("foo", classOf[String])).isFailure)
    assert(
      instanceCls.getMethod("foo", classOf[String], classOf[Int]).invoke(instance, "hello", 2: Integer) ==
        "hello2true0"
    )
    assert(
      instanceCls.getMethod("foo", classOf[String], classOf[Int], classOf[Boolean])
        .invoke(instance, "hello", 2: Integer, java.lang.Boolean.FALSE) ==
        "hello2false0"
    )
    assert(
      instanceCls.getMethod("foo", classOf[String], classOf[Int], classOf[Boolean], classOf[Long])
        .invoke(instance, "hello", 2: Integer, java.lang.Boolean.FALSE, 3: Integer) ==
        "hello2false3"
    )

    // Make sure static forwarder methods are generated
    val staticCls = Class.forName("unroll.Unrolled")
    staticCls.getMethods.filter(_.getName.contains("foo")).foreach(println)

    assert(scala.util.Try(staticCls.getMethod("foo", classOf[String])).isFailure)
    assert(
      staticCls.getMethod("foo", classOf[String], classOf[Int]).invoke(null, "hello", 2: Integer) ==
        "hello2true0"
    )
    assert(
      staticCls.getMethod("foo", classOf[String], classOf[Int], classOf[Boolean])
        .invoke(null, "hello", 2: Integer, java.lang.Boolean.FALSE) ==
        "hello2false0"
    )
    assert(
      staticCls.getMethod("foo", classOf[String], classOf[Int], classOf[Boolean], classOf[Long])
        .invoke(null, "hello", 2: Integer, java.lang.Boolean.FALSE, 3: Integer) ==
        "hello2false3"
    )

  }
}