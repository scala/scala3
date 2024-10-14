import language.experimental.captureChecking

  // ok
  def runOps(ops: List[() => Unit]): Unit =
    ops.foreach(op => op())

  // ok
  def delayedRunOps(ops: List[() => Unit]): () ->{ops*} Unit =
    () => runOps(ops)

  // unsound: impure operation pretended pure
  def delayedRunOps1(ops: List[() => Unit]): () ->{} Unit =
    () =>
      val ops1 = ops
      runOps(ops1)  // error
