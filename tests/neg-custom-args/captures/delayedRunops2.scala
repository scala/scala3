import language.experimental.captureChecking

def runOps(ops: List[() => Unit]): Unit =
  ops.foreach(op => op())

def app[T, U](x: T, op: T => U): () ->{op} U =
  () => op(x)

def unsafeRunOps(ops: List[() => Unit]): () ->{} Unit =
  app[List[() ->{ops*} Unit], Unit](ops, runOps) // error

def app2[T, U](x: T, op: T => U): () ->{op} U =
  () =>
    def y: T = x
    op(y)

def unsafeRunOps2(ops: List[() => Unit]): () -> Unit =
  app2[List[() => Unit], Unit](ops, runOps: List[() => Unit] -> Unit) // error




