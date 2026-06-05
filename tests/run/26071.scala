abstract class OutPort { self: Outlet =>
  override def hashCode: Int = super.hashCode
}

class Outlet extends OutPort

object Test:
  def main(args: Array[String]): Unit =
    val myPort = Outlet()
    println(myPort.hashCode)
