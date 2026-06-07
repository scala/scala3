import language.experimental.captureChecking
import caps.reserve
def runOps[@reserve C^](ops: List[() ->{C} Unit]): Unit = ops.foreach(_())  // error
trait Ops[C^] { def toList: List[() ->{C} Unit] }
def runOpsAlt1[@reserve C1^](ops: Ops[C1]): Unit = runOps[{C1}](???)  // ok
def runOpsAlt2[@reserve C2^](ops: Ops[{}]^{C2}): Unit = runOps[{C2}](???)  // ok
def runOpsAlt3[@reserve C3^](ops: Ops[C3]^{C3}): Unit = runOps[{C3}](???)  // ok
