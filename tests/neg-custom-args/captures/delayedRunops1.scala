import language.experimental.captureChecking

def runOps(ops: List[() => Unit]): () ->{ops*} Unit =
  () => ops.foreach(op => op())

def app[T, U](x: T, op: T => U): () ->{op} U =
  () => op(x)

def unsafeRunOps2(ops2: List[() => Unit]): () -> () -> Unit =
  val x = app[List[() ->{ops2*} Unit], () ->{ops2*} Unit](ops2, runOps) // error
  x

def unsafeRunOps3(ops2: List[() => Unit]): () -> () -> Unit =
  val x = app(ops2, runOps) // error
  x




