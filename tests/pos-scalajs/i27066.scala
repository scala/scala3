import scala.scalajs.js

object Test {
  def box(ops: js.UndefOrOps[String]): Any = ops

  def check(receiver: Any, result: Boolean): Boolean = result

  def test(value: js.UndefOr[String]): Boolean = {
    val ops = js.|.undefOr2ops(value)
    check(ops, ops.nonEmpty)
  }

  val defined: Boolean = test("x")
  val undefined: Boolean = test(js.undefined)
}
