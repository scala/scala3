import language.experimental.captureChecking
import caps.*

class Runner(val x: Int) extends AnyVal:
  def runOps[C^](ops: List[() ->{C} Unit]): Unit =
    ops.foreach(_())  // ok

class RunnerAlt(val x: Int):
  def runOps[C^](ops: List[() ->{C} Unit]): Unit =
    ops.foreach(_())  // ok, of course

class RunnerAltAlt(val x: Int) extends AnyVal:
  def runOps(ops: List[() => Unit]): Unit =
    ops.foreach(_())  // error, as expected

class RunnerAltAltAlt(val x: Int):
  def runOps(ops: List[() => Unit]): Unit =
    ops.foreach(_())  // error, as expected
