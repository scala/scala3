import caps.use

def runOps[C^](xs: List[() ->{C} Unit]): Unit = ???

def f[T <: List[() => Unit]](xs: T): () -> Unit =
  () => runOps(xs) // error

def g[T <: List[U], U <: () => Unit](xs: T): () -> Unit =
  () => runOps(xs) // error
