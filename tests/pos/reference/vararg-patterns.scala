package varargPatterns
object t1 extends App {
  val List(1, 2, xs : _*) = List(1, 2, 3)
  println(xs)
  val List(1, 2, _ : _*) = List(1, 2, 3)
}
