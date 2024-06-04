import language.experimental.captureChecking
def runOps(ops: List[() => Unit]): Unit =
  ops.foreach(op => op())

def main(): Unit =
  val f: List[() => Unit] -> Unit = runOps  // error
