class IO extends caps.SharedCapability:
  def write(): Unit = ()

class C(val io: IO):
  val c = C(io)
  val l1 = () => c.io.write()
  val _: () -> Unit = l1 // error


