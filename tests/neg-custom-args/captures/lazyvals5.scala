//> using options -Yimports:scala,scala.Predef
import language.experimental.captureChecking
import caps.*

class Console extends SharedCapability:
  def println(msg: String): Unit = Predef.println("CONSOLE: " + msg)

class IO extends SharedCapability:
  def readLine(): String = scala.io.StdIn.readLine()

class Clazz(val console: Console^):
  val io: IO^ = IO()
  lazy val memberLazy: () ->{io} String = {
    console.println("Computing memberLazy")
    () => "Member Lazy Value" + io.readLine()
  }

def client(c: Clazz^): Unit =
  val io: IO^ = IO()
  trait Trait:
    lazy val memberLazy: () ->{io} String
    def memberMethod(): String

  val t: Trait^ = ???

  lazy val funky = t.memberLazy() + c.memberLazy()

  lazy val anotherFunky =
    c.console.println("Computing anotherFunky")
    t.memberLazy

  val v0:  () -> () ->{io} String      = () => t.memberLazy     // error
  val v0_1:  () ->{t} () ->{io} String = () => t.memberLazy     // ok
  val v1:  () -> String                = () => t.memberLazy()   // error
  val v2: (() -> String)^{t}           = () => t.memberLazy()   // ok
  val v3: (() -> String)^{c.console}   = () => c.memberLazy()   // error (but should this be allowed?)
  val v4:  () -> String                = () => t.memberMethod() // error
  val v5: (() -> String)^{t}           = () => t.memberMethod() // ok

  val v6: () ->{c} String = () => funky // error
  val v6_1: () ->{t} String = () => funky // error
  val v7: () ->{c, t} String = () => funky // ok

  val v8: () ->{t, c.console} String = () => anotherFunky() // ok

  class Clazz2(val console: Console^):
    val io: IO^ = IO()
    final lazy val memberLazy: () ->{io} String = {
      console.println("Computing memberLazy")
      () => "Member Lazy Value" + io.readLine()
    }

  trait Trait2:
    final lazy val memberLazy : () ->{io} String = () => io.readLine()

  val c2: Clazz2^ = ???
  val t2: Trait2^ = ???

  lazy val funky2 = t2.memberLazy() + c2.memberLazy()

  val v9: () ->{c2.memberLazy, t2.memberLazy} String = () => funky2 // error (but should this be allowed?)
  val v10: () ->{t2, c2} String = () => funky2 // ok

  ()
