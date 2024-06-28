

object Test1 {
  trait Tree[-T]

  class JavaSeqLiteral[T] extends Tree[T]

  trait Type

  class DummyTree extends JavaSeqLiteral[Any]

  def foo1(tree: Tree[Type]) =
    tree.isInstanceOf[JavaSeqLiteral[Type]]   // warn

  foo1(new DummyTree)
}

object Test2 {
  trait Tree[-T]

  class JavaSeqLiteral[-T] extends Tree[T]

  trait Type

  class DummyTree extends JavaSeqLiteral[Any]

  def foo1(tree: Tree[Type]) =
    tree.isInstanceOf[JavaSeqLiteral[Type]]

  foo1(new DummyTree)
}
