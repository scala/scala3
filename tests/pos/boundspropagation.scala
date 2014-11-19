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
      case y: Tree[_] => y // now succeeds in dotc
    }
  }
}

