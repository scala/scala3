

object Test1 {
  trait Tree
  trait Context

  def foo1(myTree: Tree | (Context => Tree)) =
    println(myTree.isInstanceOf[Tree])

  def foo2(myTree: Tree | (Context => Tree)) =
    myTree match
      case treeFn: (Context => Tree) =>  // warn
      case _ =>

  def foo3(myTree: Tree | (Context => Tree)) =
    myTree match
      case treeFn: (? => ?) =>       // ok
      case _ =>
}

object Test2 {
  trait Tree[-T]
  trait Context

  trait Type

  def foo1(myTree: Tree[Type] | (Context => Tree[Type])) =
    println(myTree.isInstanceOf[Tree[Type]])   // warn
    /* class DummyTree extends Tree[Nothing] with (Context => Tree[Type]) */

  def foo2(myTree: Tree[Type] | (Context => Tree[Type])) =
    myTree match
      case treeFn: (Context => Tree[Type]) =>  // warn
      case _ =>

  def foo3(myTree: Tree[Type] | (Context => Tree[Type])) =
    myTree match
      case treeFn: (? => ?) =>       // ok
      case _ =>
}