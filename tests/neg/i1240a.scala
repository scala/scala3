
// more complicated example
abstract class A {
  type C[X]
  def foo[B](x: C[B]): C[B]       = {println("A.C"); x}
  def foo[B](x: List[B]): List[B] = {println("A.List"); x}
  def give[X]: C[X]
}

class B extends A {
  type C[X] = List[X]
  override def give[X] = Nil
  override def foo[B](x: C[B]): C[B] =  {println("B.C"); x}
  val a: A = this
  a.foo(a.give[Int]) // what method should be called here in runtime? // error: ambiguous
}

