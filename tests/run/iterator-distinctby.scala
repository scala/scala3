object Test {
  def main(args: Array[String]): Unit = {
    // basic distinctBy
    val xs = List(1, 2, 2, 3, 3, 3).iterator.distinctBy(identity)
    assert(xs.toList == List(1, 2, 3), "basic distinctBy failed")

    // distinctBy with transformation
    val ys = List("apple", "ant", "banana", "avocado").iterator.distinctBy(_.head)
    assert(ys.toList == List("apple", "banana"), "distinctBy with transformation failed")

    // distinctBy on empty iterator
    val zs = Iterator.empty[Int].distinctBy(identity)
    assert(zs.toList == Nil, "distinctBy on empty iterator failed")

    // distinctBy with all duplicates - regression test for stack overflow
    // with consecutive duplicates (was a bug in hasNext recursion)
    val N = 100000
    val allSame = Array.fill(N)(1).iterator.distinctBy(identity)
    assert(allSame.toList == List(1), "distinctBy with all duplicates failed (stack overflow?)")

    // distinctBy with consecutive duplicates at start
    val consec = (List.fill(50000)(1) ++ List(2, 3)).iterator.distinctBy(identity)
    assert(consec.toList == List(1, 2, 3), "distinctBy with consecutive duplicates at start failed")

    println("OK")
  }
}
