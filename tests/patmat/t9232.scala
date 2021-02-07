final class Foo(val value: Int)

object Foo {
  def unapplySeq(foo: Foo): Some[Seq[Int]] = Some(List(foo.value))
  // def unapply(foo: Foo): Some[Int] = Some(foo.value)
}

sealed trait Tree
case class Node1(foo: Foo) extends Tree
case class Node2() extends Tree

object Test {
  def transformTree(tree: Tree): Any = tree match {
    case Node1(Foo(_*)) => ???
  }

  def transformTree2(tree: Tree): Any = tree match {
    case Node1(Foo(1, _*)) => ???
  }

  def transformTree3(tree: Tree): Any = tree match {
    case Node1(Foo(x, _*)) => ???
  }
}
