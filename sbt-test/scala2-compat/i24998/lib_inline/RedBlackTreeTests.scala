package scala.collection.immutable
package test

object ReadBlackTreeTests {
  private def sumKeys(t: RedBlackTree.Tree[Int, String]): Int =
    if (t eq null) 0
    else t.key + sumKeys(t.left) + sumKeys(t.right)

  private def collectValues(t: RedBlackTree.Tree[Int, String]): List[String] =
    if (t eq null) Nil
    else t.value :: collectValues(t.left) ::: collectValues(t.right)

  def apply(): Unit = {
    val t1 = new RedBlackTree.Tree[Int, String](1, "value", null, null, 0)
    val t2 = new RedBlackTree.Tree[Int, String](2, "two", null, null, 0)
    val t3 = new RedBlackTree.Tree[Int, String](3, "three", t1, t2, 1)

    assert(t1.key == 1)
    assert(t1.value == "value")
    assert(t3.left eq t1)
    assert(t3.right eq t2)
    assert(sumKeys(t3) == 6)
    assert(collectValues(t3).sorted == List("three", "two", "value"))
    val (left, right) = RedBlackTree.partitionEntries(t3, { (key: Int, value: String) => key % 2 == 0 })
    assert(left.key == 2)
    assert(right.key == 3)
  }
}
