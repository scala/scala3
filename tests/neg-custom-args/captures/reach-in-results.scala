import language.experimental.captureChecking
trait File
def usingFile[R](op: File^ => R): R = op(new File {})
def id(x: File^): File^{x} = x
def test1(): Unit =
  val op: File^ -> File^ = id // error, this should be disallowed
  val g: File^ -> File^{op*} = op  // otherwise we can brand arbitrary File as simply capturing op*