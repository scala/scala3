object Test {
  def main(args: Array[String]): Unit = {
    val xs = List(1, 2, 2, 3, 3, 3).iterator.distinctBy(identity)
    assert(xs.toList == List(1, 2, 3))

    val ys = List("apple", "ant", "banana", "avocado").iterator.distinctBy(_.head)
    assert(ys.toList == List("apple", "banana"))

    val zs = Iterator.empty[Int].distinctBy(identity)
    assert(zs.toList == Nil)

    // regression: stack overflow on consecutive duplicates
    val allSame = Array.fill(100000)(1).iterator.distinctBy(identity)
    assert(allSame.toList == List(1))

    val consec = (List.fill(50000)(1) ++ List(2, 3)).iterator.distinctBy(identity)
    assert(consec.toList == List(1, 2, 3))
  }
}
