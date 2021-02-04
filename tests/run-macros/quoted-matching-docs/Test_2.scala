object Test extends App {
  println(sumShow(1, 2, 3))
  println(sum(1, 2, 3))
  val a: Int = 5
  println(sumShow(1, a, 4, 5))
  println(sum(1, a, 4, 5))
  val seq: Seq[Int] = Seq(1, 3, 5)
  println(sumShow(seq*))
  println(sum(seq*))
}
