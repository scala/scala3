// This test is based on tests/pos/rbtree.scala
// and it tests that we can use an inline method to "abstract" a more complicated
// isInstanceOf check, while at the same time getting the flow inference to know
// that `isRedTree(tree) => tree ne null`.

class TreeOps {
  abstract class Tree[A, B](val key: A, val value: B)
  class RedTree[A, B](override val key: A, override val value: B) extends Tree[A, B](key, value)

  private transparent inline def isRedTree(tree: Tree[_, _] | Null) =
    (tree != null) && tree.isInstanceOf[RedTree[_, _]]

  def foo[A, B](tree: Tree[A, B] | Null): Unit = {
    if (isRedTree(tree)) {
      val key = tree.key
      val value = tree.value
    }

    if (!isRedTree(tree)) {
    } else {
      val key = tree.key
      val value = tree.value
    }
  }
}
