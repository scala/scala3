import caps.*
class IO extends SharedCapability:
  def println(msg: String): Unit = ()
object IO extends SharedCapability:
  val io: IO = new IO
  def assertPure(op: () -> Unit): Unit = ()
