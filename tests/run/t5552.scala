class C[@specialized(Int) A](a:A) {
  lazy val b = (a, a)
  def c = b
}
object Test {
  def main(args:Array[String]): Unit = {
    println(new C(3).c)
    println(new C(3.5).c)
  }
}
