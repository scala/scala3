trait Settings:
  inline def switch: Boolean = true

trait Inner:
  def switch: Boolean
  inline def go1: String = inline if switch then "Yes" else "No"
  def go2: String = if switch then "Yes" else "No"

object Outer extends Inner, Settings:
  def test1 = go1
  def test2 = go2

@main def main(): Unit =
  println(Outer.test1)
  println(Outer.test2)
  val a: Inner = Outer
  println(a.switch)
