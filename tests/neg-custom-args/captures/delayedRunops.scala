import language.experimental.captureChecking
import caps.unbox

  // ok
  def runOps(@unbox ops: List[() => Unit]): Unit =
    ops.foreach(op => op())

  // ok
  def delayedRunOps(@unbox ops: List[() => Unit]): () ->{ops*} Unit = // @unbox should not be necessary in the future
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
