class TestSuite:
  protected val it = new ItWord

  protected final class ItWord:
    def should(string: String) = new ItVerbString("should", string)

  private def registerTestToRun(fun: => Any): Unit = ()

  protected final class ItVerbString(verb: String, name: String):
    inline def in(testFun: => Any): Unit = registerTestToRun(testFun)

class MyTest extends TestSuite:
  it should "not cause outer select errors" in {
    assert(1 + 1 == 2)
  }

  val n = 10
