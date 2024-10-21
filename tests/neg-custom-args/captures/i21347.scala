import language.experimental.captureChecking

def runOps[C^](ops: List[() ->{C^} Unit]): Unit =
  ops.foreach: op =>
    op()

def boom(f: () => Unit): () -> Unit =
  () => runOps(f :: Nil) // error

def runOpsAlt(ops: List[() => Unit]): Unit =
  ops.foreach: op =>
    op()
