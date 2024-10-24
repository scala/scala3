import language.experimental.captureChecking

def runOps(ops: List[() => Unit]): Unit =
  ops.foreach(op => op())

def app[T, U](x: T, op: T => U): () ->{op} U =
  () => op(x)

def test(c: Object^) =

  def unsafeRunOps1(ops: List[() ->{c} Unit]): () -> Unit =
    app[List[() ->{c} Unit], Unit](ops, runOps) // !!! ok, but should be error

  def unsafeRunOps2(ops: List[() ->{c} Unit]): () -> Unit =
    app(ops, runOps) // error

  ()




