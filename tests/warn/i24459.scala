//> using options -Werror -Wunused:all
///> using scala 3.8.0-RC1

trait Suite {
  inline def test(body: => Any): Any = body
}

object Test {
  private case class I()
}

class Test extends Suite {
  test {
    val i = Test.I()
    i.hashCode
  }
}
