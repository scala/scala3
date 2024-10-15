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

  // unsound: impure operation pretended pure
  def delayedRunOps2(ops: List[() => Unit]): () ->{} Unit =
    () =>
      val ops1: List[() => Unit] = ops
      runOps(ops1)  // error

  // unsound: impure operation pretended pure
  def delayedRunOps3(ops: List[() => Unit]): () ->{} Unit =
    () =>
      val ops1: List[() ->{ops*} Unit] = ops
      runOps(ops1)  // error
