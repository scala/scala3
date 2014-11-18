class A[T] {

  def f(x: T)(y: T = x) = y

}

class B extends A[Int] {

  override def f(x: Int)(y: Int) = y

  f(2)()

}

class A1
class A2
class X1 {
  type T = A1
}
class Y1 extends X1 {
  override type T = A2
}
