final class Foo(val value: Int)

object Foo {
  inline def unapplySeq(foo: Foo): Some[Seq[Int]] = Some(List(foo.value))
}

sealed trait Tree
case class Node1(foo: Foo) extends Tree
case class Node2() extends Tree

object Test {
  def transformTree(tree: Tree): Any = tree match {
    case Node1(Foo(_: _*)) => ???
  }

  def transformTree2(tree: Tree): Any = tree match {
    case Node1(Foo(1, _: _*)) => ???
  }

  def transformTree3(tree: Tree): Any = tree match {
    case Node1(Foo(x, _: _*)) => ???
  }
}
