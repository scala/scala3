import language.experimental.captureChecking
trait IO { def println(msg: String): Unit }
def test1(ops: List[() => Unit]): Unit = ops.foreach(op => op())  // error
def test2(ops: List[() => Unit]): () ->{} Unit = () => ops.foreach(f => f())  // error
def test3(ops: List[() => Unit]): () ->{ops*} Unit = () => ops.foreach(f => f())  // ok
