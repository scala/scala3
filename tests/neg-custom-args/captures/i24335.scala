class IO extends caps.SharedCapability:
  def write(): Unit = ()

class C(val io: IO):
  val c = C(io)
  val l1 = () => c.io.write()
  val _: () -> Unit = l1 // error

class Ref extends caps.Mutable:
  var x: Int = 0

class D:
  val r = Ref() // error

def test =
  val d = D()
  val _: D^{} = d

