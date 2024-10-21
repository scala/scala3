import language.experimental.captureChecking

def runOps[C^](ops: List[() ->{C^} Unit]): Unit =
  ops.foreach(op => op())

def app[T, U](x: T, op: T -> U): () ->{} U =
  () => op(x)

def unsafeRunOps[C^](ops: List[() ->{C^} Unit]): () ->{} Unit =
  app[List[() ->{C^} Unit], Unit](ops,
    runOps[C]: List[() ->{C^} Unit] ->{C^} Unit) // error

def unsafeRunOps2[C^](ops: List[() ->{C^} Unit]): () ->{} Unit =
  def rops[D^]: (ops: List[() ->{D^} Unit]) -> Unit = ???
  app[List[() ->{C^} Unit], Unit](ops, rops[C]) // error

def unsafeRunOps3[C^](ops: List[() ->{C^} Unit]): () ->{} Unit =
  app[List[() ->{C^} Unit], Unit](ops, runOps) // error






