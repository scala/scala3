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

// Example contributed by Jason. I believe this should not typecheck,
// even though scalac does typecheck it.
object test4 {
  class Base {
    type N

    class Tree[-S, -T >: Option[S]]

    def g(x: Any): Tree[_, _ <: Option[N]] = x match {
      case y: Tree[_, _] => y                         // error
    }
  }
}

class Test5 {
"": ({ type U = this.type })#U                        // error
}
