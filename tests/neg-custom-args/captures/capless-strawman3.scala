import language.experimental.captureChecking
trait IO { def println(msg: String): Unit }
def test1(io1: IO^, io2: IO^, op: () -> () ->{io1} () ->{io2} Unit): Unit =
  val t1: () -> Unit = () => op()
  val t2: () -> Unit = () => op()()  // error
  val t2a: () ->{io1} Unit = () => op()()
  val t3: () -> Unit = () => op()()()  // error
  val t3a: () ->{io2} Unit = () => op()()()  // error
  val t3b: () ->{io1, io2} Unit = () => op()()()
def test2(io1: IO^, io2: IO^, op: () -> () ->{io1} () ->{io2} Unit): Unit =
  val t1: () -> Unit = () =>  // error
    val f = op()
    f()
