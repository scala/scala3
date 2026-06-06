import language.experimental.captureChecking

  def runOps[C^](ops: List[() ->{C} Unit]): Unit =
    ops.foreach(op => op())

  // ok
  def delayedRunOps[C^](ops: List[() ->{C} Unit]): () ->{C} Unit = // @use should not be necessary in the future
    () => runOps(ops)

  // unsound: impure operation pretended pure
  def delayedRunOps1(ops: List[() => Unit]): () ->{} Unit =
    () => // error
      val ops1 = ops
      runOps(ops1)

  // unsound: impure operation pretended pure
  def delayedRunOps3[c^](ops: List[() ->{c} Unit]): () ->{} Unit =
    () => // error
      val ops1: List[() ->{c} Unit] = ops
      runOps(ops1)
