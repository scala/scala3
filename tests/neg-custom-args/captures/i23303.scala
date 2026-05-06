import language.experimental.captureChecking
import caps.use

class Test:
  class Runner(ops: List[() => Unit]):
    def execute: Unit = ops.foreach(f => f()) // error

  def Runner2(ops: List[() => Unit]) =
    () => ops.foreach(f => f()) // error


class Test2:
  class Runner[C^](ops: List[() ->{C} Unit]):
    def execute: Unit = ops.foreach(f => f()) //ok

  private def Runner2[C^](ops: List[() ->{C} Unit]) =
    () => ops.foreach(f => f()) // ok
