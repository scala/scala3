package test

class C[T] {

    def foo(x: D) = { System.out.println("D foo"); }
    def foo(x: T) = { System.out.println("T foo"); }
}

object C {
  def main(args: Array[String]) =
        new C[D]().foo(new D()) // error: ambiguous
}
/*
class C1[T] {
    def foo(x: D) = { System.out.println("D foo"); }
}
class C2[T] {
    def foo(x: D) = { System.out.println("D foo"); }
}

class D {}

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
                                           // which method is overriden?
                                           // should any bridges be generated?
  val a: A = this
  a.foo(a.give[Int]) // what method should be called here in runtime?
}
*/
