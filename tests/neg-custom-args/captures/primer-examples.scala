
import caps.*
class File(path: String) extends ExclusiveCapability:
  def close(): Unit = ()

class Reader(f: File):
  def read: Int = ???

class Logger(f: File):
  def log(msg: String): Unit = ???

val in = File("in")
val reader: Reader^{in.rd} = Reader(in)

val file = File("~/last.log")
val logger: Logger^{file} = Logger(file)

def withFile[T](op: File^ => T): T =
  val f = File("path")
  val result = op(f)
  f.close()
  result

def logged[T](op: Logger^ => T): T =
  val f = new File("logfile")
  val l = Logger(f)
  val result = op(l)
  f.close()
  result

val bad = logged(l => () => l.log("too late!")) // error
val _ = bad()

def test =
  var counter: Int = 0
  def incr = () => { counter += 1; counter }
  def current = () => counter
  type Proc = () => Int
  def par(p: Proc, q: Proc) = ()
  par(incr, current) // error
  par(current, current)
  incr()
  current()

