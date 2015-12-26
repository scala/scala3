class Num[T] {
  def mkOps = new Ops
  class Ops { def +++(rhs: T) = () }
}

class A {
  implicit def infixOps[T, CC[X] <: Num[X]](lhs: T)(implicit num: CC[T]): num.Ops = num.mkOps
  implicit val n1: Num[Int] = new Num[Int] { }
  println(5 +++ 5) // should dependent be implicits forbidden?
}

class B {
  implicit def infixOps[T, CC[X] <: Num[X]](lhs: T)(implicit num: CC[T]) : CC[T]#Ops = num.mkOps
  implicit val n1: Num[Int] = new Num[Int] {}
  println(5 +++ 5)
}
