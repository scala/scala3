import language.experimental.captureChecking

val runOps: [C^] -> () -> (ops: List[() ->{C^} Unit]) ->{C^} Unit = ???

def app[T, U](x: T, op: T => U): () ->{op} U =
  () => op(x)

def unsafeRunOps(ops: List[() => Unit]): () ->{} Unit =
  app[List[() ->{ops*} Unit], Unit](ops, runOps()) // error

def unsafeRunOps2(ops: List[() => Unit]): () ->{} Unit =
  app(ops, runOps()) // error

def test(c: Object^) =
  def f = (ops: List[() ->{c} Unit]) => ops.foreach(_())
  val _: List[() ->{c} Unit] ->{c} Unit = f
  ()
