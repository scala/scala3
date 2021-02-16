package varargPatterns
object t1 extends App {
  val List(1, 2, xs *) = List(1, 2, 3)
  println(xs)
  val List(1, 2, _ *) = List(1, 2, 3)
}
@main def Test =
  val arr = Array(0, 1, 2, 3)
  val lst = List(arr*)                  // vararg splice argument
  lst match
    case List(0, 1, xs*) => println(xs)   // binds xs to Seq(2, 3)
    case List(1, _*) =>                   // wildcard pattern
