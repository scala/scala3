case class Box[T](x: T):
  def foreach(f: T => Unit): Unit = f(x)

def runOps(ops: Box[() => Unit]): () -> Unit =
  val applyFn: (() => Unit) -> Unit = f => f()
  val fn: () -> Unit = () =>
    ops.foreach(applyFn)  // error
  fn
