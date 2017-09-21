object A {
  def fun[E >: B](a: A[E]): E => Unit = ???
  val x = fun(new A[C])
}
class B extends C
class C

class A[-X >: B]
