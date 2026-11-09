
class C {
  def k = {
    object + extends Function1[Int, Int] { def apply(i: Int): Int = i + 1 }
    val g: Int => Int = +
    g(1)
  }
  def ok = {
    val i = 42
    val n = +i
    n
  }
}
