import language.experimental.captureChecking
trait IO { def println(msg: String): Unit }
def test1(c: IO^): Unit =
  val f: () -> () ->{c} Unit = () => () => c.println("hello")  // ok
  val f2: () -> Unit = () => f()()  // error
  val f3: () ->{c} Unit = () => f()()  // ok
