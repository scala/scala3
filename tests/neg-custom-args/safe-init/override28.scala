abstract class Base(val x: Int) {
  def f: Int = 10
  val a = x
}


class C(x: Int) extends Base(x) {
  val d = f     // error
}