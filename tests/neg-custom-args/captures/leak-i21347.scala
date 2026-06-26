import language.experimental.captureChecking

def runOpsAlt(ops: List[() => Unit]): Unit =
  ops.foreach: op => // error
    op()