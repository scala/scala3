import language.experimental.captureChecking
import caps.*

class Console extends SharedCapability:
  def println(msg: String): Unit = Predef.println("CONSOLE: " + msg)

class IO extends SharedCapability:
  def readLine(): String = scala.io.StdIn.readLine()

class Clazz(val console: Console^):
  lazy val memberLazy: () -> String = {
    console.println("Computing memberLazy")
    () => "Member Lazy Value"
  }

trait Trait:
  lazy val memberLazy: () -> String
  def memberMethod(): String

def client(t: Trait^, c: Clazz^): Unit =
  val v0:  () -> () -> String        = () => t.memberLazy     // error
  val v0_1:  () ->{t} () -> String   = () => t.memberLazy     // ok
  val v1:  () -> String              = () => t.memberLazy()   // error
  val v2: (() -> String)^{t}         = () => t.memberLazy()   // ok
  val v3: (() -> String)^{c.console} = () => c.memberLazy()   // error (but should this be allowed?)
  val v4:  () -> String              = () => t.memberMethod() // error
  val v5: (() -> String)^{t}         = () => t.memberMethod() // ok

  ()