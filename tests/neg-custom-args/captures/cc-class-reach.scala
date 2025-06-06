import language.experimental.captureChecking
import caps.*
class Runner(@use xs: List[() => Unit]):
  def execute: Unit = xs.foreach(op => op())
def test1(@use ops: List[() => Unit]): Unit =
  val runner: Runner^{} = Runner(ops)  // error

