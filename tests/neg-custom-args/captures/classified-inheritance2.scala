import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*
class Logger extends SharedCapability, Mutable:  // error (1) does this make sense?
  private var _state: Int = 0
  update def log(msg: String): Unit = ()

def onlyShared(x: Object^{cap.only[SharedCapability]}): Unit = ()

def main(): Unit =
  onlyShared(Logger())  // even if we allow (1), why would this type check?
  val t: Logger^{} = Logger()  // and this type checks too, thus the above line I guess