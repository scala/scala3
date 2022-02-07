import scala.compiletime.*

object Test {
  import Logging.*

  def main(args: Array[String]): Unit = {
     runLog()
     runBasic()
  }

  def runLog(): Unit = {
    trace("I'm a trace msg")
    debug("I'm a debug msg")
    info("I'm a info msg")
    warn("I'm a warn msg")
  }

  def runBasic(): Unit = {
    printEnv("a")    
    printEnv("b")    
    printEnv("c.b.a")    
    printEnv("wat")    
  }

  inline def printEnv(inline k: String): Unit =
    inline MacroEnv.get(k) match
      case Some(v) => println(s"$k = [$v]")
      case None    => println(k + " is not defined")

}
