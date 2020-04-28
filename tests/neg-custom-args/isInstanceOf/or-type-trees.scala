trait Tree
trait Context

def foo(myTree: Tree | (Context => Tree)) =
  println(myTree.isInstanceOf[Tree])
  myTree match
    case treeFn: (Context => Tree) =>  // error
    case _ =>

def bar(myTree: Tree | (Context => Tree)) =
  myTree match
    case treeFn: (_ => _) =>       // ok
    case _ =>
