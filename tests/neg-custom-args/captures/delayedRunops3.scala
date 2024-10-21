import language.experimental.captureChecking

def runOps(ops: List[() => Unit]): Unit =
  ops.foreach(op => op())

def app[T, U](x: T, op: T -> U): () ->{} U =
  () => op(x)

def unsafeRunOps(ops: List[() => Unit]): () ->{} Unit =
  app[List[() ->{ops*} Unit], Unit](ops, runOps) // error




