object Test extends App {
  println(showOptimize(sum(1, 2, 3)))
  println(optimize(sum(1, 2, 3)))
  val a: Int = 5
  println(showOptimize(sum(1, a, sum(1, 2, 3), 5)))
  println(optimize(sum(1, a, sum(1, 2, 3), 5)))
  val seq: Seq[Int] = Seq(1, 3, 5)
  println(showOptimize(sum(1, sum(seq*), 3)))
  println(optimize(sum(1, sum(seq*), 3)))
}
