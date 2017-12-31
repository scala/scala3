trait T {
  @org.junit.Test def foo = 0
  println("hi")  // to force an $init method
}

trait U {
  @org.junit.Test def bar = 0
    // don't issue a $init method
}


class C extends T

object Test extends App {
  def check(c: Class[_], e: String) = {
    val s = c.getDeclaredMethods.sortBy(_.getName).map(m => s"${m.getName} - ${m.getDeclaredAnnotations.mkString(", ")}").mkString(";")
    assert(s == e, s"found: $s\nexpected: $e")
  }
  check(classOf[C], "foo - @org.junit.Test()")
  // TODO scala-dev#213: should `foo$` really carry the @Test annotation?
  check(classOf[T], "$init$ - ;foo - @org.junit.Test()")
  check(classOf[U], "bar - @org.junit.Test()")
}
