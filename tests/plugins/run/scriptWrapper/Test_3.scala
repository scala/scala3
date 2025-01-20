@main def Test: Unit = {
  val mainCls = Class.forName("foo_sc")
  val mainMethod = mainCls.getMethod("main", classOf[Array[String]])
  val stackTrace: Array[String] = {
    try
      mainMethod.invoke(null, Array.empty[String])
      sys.error("Expected an exception")
    catch
      case e: java.lang.reflect.InvocationTargetException =>
        val cause = e.getCause
        if cause != null then
          cause.getStackTrace.map(_.toString)
        else
          throw e
  }

  val expected = Set(
    "foo_sc$.getRandom(foo_2.scala:3)", // adjusted line number (11 -> 3)
    "foo_sc$.brokenRandom(foo_2.scala:5)", // adjusted line number (13 -> 5)
    "foo_sc$.run(foo_2.scala:8)", // adjusted line number (16 -> 8)
  )

  val missing = expected -- stackTrace
  assert(missing.isEmpty, s"Missing: $missing")
}
