// scalac fails for test2/3
// dotc fails for all three
object test1 {
  class Tree[-T >: Null]


  def f(x: Any): Tree[Null] = x match {
    case y: Tree[_] => y
  }
}
object test2 {
  class Tree[T >: Null]


  def f(x: Any): Tree[Null] = x match {
    case y: Tree[_] => y                              // error
  }
}
object test3 {
  class Tree[+T >: Null]


  def f(x: Any): Tree[Null] = x match {
    case y: Tree[_] => y                              // error
  }
}

class Test5 {
"": ({ type U = this.type })#U                        // error
}
