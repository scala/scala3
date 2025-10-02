import language.experimental.captureChecking
trait IO { def println(msg: String): Unit }
abstract class Runner:
  def run(): Unit
def test1(ops: List[() => Unit]): Runner^{ops*} =
  new Runner:
    def run(): Unit = ops.foreach(op => op())
def test2(ops: List[() => Unit]): Runner^{} =
  new Runner:
    def run(): Unit = ops.foreach(op => op())  // error
def test3(op: () => Unit): Runner^{} =
  new Runner:
    def run(): Unit = op()  // error
