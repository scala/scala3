import language.experimental.captureChecking

  def runOps[C^](ops: List[() ->{C} Unit]): Unit =
    ops.foreach(op => op())

  // ok
  def delayedRunOps[C^](ops: List[() ->{C} Unit]): () ->{C} Unit = // @use should not be necessary in the future
    () => runOps(ops)

 // unsound: impure operation pretended pure
  def delayedRunOps1(ops: List[() => Unit]): () ->{} Unit =
    () =>
      val ops1 = ops
      runOps(ops1) // error

// unsound: impure operation pretended pure
  def delayedRunOps2(consume ops: List[() => Unit]): () ->{} Unit =
    () =>
      val ops1: List[() => Unit] = ops // was error, ok after reach drop
      runOps(ops1)  // error

