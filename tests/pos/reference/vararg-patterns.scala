package varargPatterns
object t1 extends App {
  val List(1, 2, xs *) = List(1, 2, 3)
  println(xs)
  val List(1, 2, _ *) = List(1, 2, 3)
}
