// scalajs: --skip

import scala.annotation.varargs

class VarargImpl extends VarargAbstract {
  def v1(s: String*) = ()

  override def v2(s: String*) = ()
}

class VarargSub extends Vararg {
  override def v(i: Int*) = ()
}

object Test {
  def main(args: Array[String]): Unit =
    val a: VarargAbstract = VarargImpl()
    a.v1("a", "b", "c")
    a.v2("a", "b", "c")

    val i: VarargImpl = VarargImpl()
    i.v1("a", "b", "c")
    i.v2("a", "b", "c")

    val b: Vararg = VarargSub()
    b.v(1, 2, 3)

    val c: VarargSub = VarargSub()
    c.v(1, 2, 3)
}
