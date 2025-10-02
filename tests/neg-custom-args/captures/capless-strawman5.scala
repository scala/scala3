import language.experimental.captureChecking
trait Lazy[+T]:
  def eval: T
trait IO { def println(msg: String): Unit }
def flatMap[A, B](x: Lazy[A]^, f: A => Lazy[B]^): Lazy[B]^{x, f*} =  // ok
  new Lazy[B] { def eval: B = f(x.eval).eval }
def test1[A, B](x: Lazy[A]^, f: A => Lazy[B]^): B =  // error
  f(x.eval).eval
def test2(newCap: (z: Int) -> IO^): Unit =
  val f: () -> Unit =
    val myIO = newCap(42)
    () => myIO.println("hello")  // error
