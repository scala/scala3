import language.experimental.captureChecking
def runOps[C^](ops: List[() ->{C} Unit]): Unit = ops.foreach(_())  // ok
trait Ops[C^] { def toList: List[() ->{C} Unit] }
def runOpsAlt1[C1^](ops: Ops[C1]): Unit = runOps[{C1}](???)  // was error, now ok
def runOpsAlt2[C2^](ops: Ops[{}]^{C2}): Unit = runOps[{C2}](???)  // ok
def runOpsAlt3[C3^](ops: Ops[C3]^{C3}): Unit = runOps[{C3}](???)  // was error, no ok
