import caps.*

class Logger extends SharedCapability:
  def log(msg: String): Unit = println(msg)

class Ref(init: Int)(using l: Logger) extends Stateful:
  self: Ref^ =>
  private var current = init
  def get(): Int =
    l.log("get")
    current
  update def set(x: Int) =
    l.log("set $x")
    current = x

def test =
  given l: Logger = Logger()
  val r: Ref^{any.rd} = Ref(3)
  r.get()
