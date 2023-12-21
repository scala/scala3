abstract class Base(val x: Int) {
  val d: Int
  def f: Int = d
  val a = x
}


class C(x: Int) extends Base(x) {
  val d = f                        // warn
}