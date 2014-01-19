object test {
 
  abstract class Tree[-T0 >: Null]
  
  trait TermTree[-T1 >: Null] extends Tree[T1]
  
  case class Literal[-T2 >: Null](x: Tree[T2]) extends /*Tree[T2] with*/ TermTree[T2]

  def f[T >: Null](x: Tree[T]): Unit = {
    def g(x: Tree[T]): Unit = ()
    x match {
      case Literal(x) => g(x)
    }
  }
}