case class X(val x: Int) extends AnyVal
case class Y(val y: Any) extends AnyVal
object Test2 {
  def t: AnyVal = new X(5)
  def t2 = X(5)
  def w: AnyVal = new Y(7)
  def w2 = Y(7)
  def main(args: Array[String]) = {
    println(t)
    println(t2)
    println(w)
    println(w2)
  }
}