import language.experimental.captureChecking

def runOps[cap C](ops: List[() ->{C} Unit]): Unit =
  ops.foreach: op => // error
    op()

def boom(f: () => Unit): () -> Unit =
  () => runOps(f :: Nil) // now ok

def runOpsAlt(ops: List[() => Unit]): Unit =
  ops.foreach: op => // error
    op()