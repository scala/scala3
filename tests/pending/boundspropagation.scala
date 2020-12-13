// test contributed by @retronym
object test1 {
  class Base {
    type N

    class Tree[-T >: N]

    def f(x: Any): Tree[N] = x match {
      case y: Tree[_] => y
    }
  }
  class Derived extends Base {
    def g(x: Any): Tree[N] = x match {
      case y: Tree[_] => y.asInstanceOf
        // without the cast: fails in scalac and new dotc
        // used to succeed in dotc if type args are refinements
    }
  }
}

/** Does not work:
object test2 {
  class Tree[S, T <: S]

  class Base {
    def g(x: Any): Tree[_, _ <: Int] = x match {
      case y: Tree[Int @unchecked, t] => y
    }
  }
}
*/

// Example contributed by Jason.
object test2 {
  class Base {
    type N

    class Tree[-S, -T >: Option[S]]

    def g(x: Any): Tree[_, _ <: Option[N]] = x match {
      case y: Tree[_, _] => y
    }
  }
}
