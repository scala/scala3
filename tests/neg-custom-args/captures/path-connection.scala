import language.experimental.modularity

trait Reader:
  def read(): String

trait Sender:
  def send(msg: String): Unit

class Connection extends Reader, Sender:
  def read() = "hello"
  def send(msg: String) = ()

  val readOnly: Reader^ = new Reader:
    def read() = Connection.this.read()

class ReaderProxy(tracked val r: Reader^) extends Reader:
  def read() = "(Proxy)" + r.read()

class SenderProxy(tracked val s: Sender^) extends Sender:
  def send(msg: String) = s.send("(Proxy) " + msg)

// TODO: We have to put `c` in the different argument list to make it work.
// See the comments in `integrateRT`.
def testConnection(c: Connection^)(
    handle1: Reader^{c.readOnly} => String,
    handle2: Sender^{c} => Unit,
    handle3: Reader^{c} => String,
    ) =
  val m1 = c.read()
  c.send("hello")

  val m2 = c.readOnly.read()

  val m3a = handle1(c.readOnly)
  val m3b = handle3(c.readOnly)

  val m4a = handle1(c) // error
  val m4b = handle3(c)

  val m5a = handle1(new ReaderProxy(c.readOnly))
  val m5b = handle3(new ReaderProxy(c.readOnly))

  val m6a = handle1(new ReaderProxy(c)) // error
  val m6b = handle3(new ReaderProxy(c))

  handle2(c)

  handle2(new SenderProxy(c))