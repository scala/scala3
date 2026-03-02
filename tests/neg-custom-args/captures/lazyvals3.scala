import language.experimental.captureChecking
import caps.*

class C
type Cap = C^

class Console extends SharedCapability:
  def println(msg: String): Unit = Predef.println("CONSOLE: " + msg)

class IO extends SharedCapability:
  def readLine(): String = scala.io.StdIn.readLine()

def test(c: Cap, console: Console^, io: IO^): Unit =
  lazy val ev: (Int -> Boolean) = (n: Int) =>
    lazy val od: (Int -> Boolean) = (n: Int) =>
      if n == 1 then true else ev(n - 1)
    if n == 0 then true else od(n - 1)

  // In a mutually recursive lazy val, the result types accumulate the captures of both the initializers and results themselves.
  // So, this is not ok:
  lazy val ev1: (Int ->{io,console} Boolean) =
    println(c)
    (n: Int) =>
      lazy val od1: (Int ->{ev1,console} Boolean) = (n: Int) => // error
        if n == 1 then
          console.println("CONSOLE: 1")
          true
        else
          ev1(n - 1)
      if n == 0 then
        io.readLine() // just to capture io
        true
      else
        od1(n - 1)

  // But this is ok:
  lazy val ev2: (Int ->{c,io,console} Boolean) =
    println(c)
    (n: Int) =>
      lazy val od2: (Int ->{c,io,console} Boolean) = (n: Int) =>
        if n == 1 then
          console.println("CONSOLE: 1")
          true
        else
          ev2(n - 1)
      if n == 0 then
        io.readLine() // just to capture io
        true
      else
        od2(n - 1)

  val even: Int -> Boolean = (n: Int) => ev(n) // ok
  val even2: Int ->{io,console,c} Boolean = (n: Int) => ev1(n) // ok

  ()