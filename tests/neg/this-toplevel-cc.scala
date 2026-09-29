import language.experimental.captureChecking
import caps.*

class File extends SharedCapability

def foo() =
  val f: File^{this} = ??? // error
