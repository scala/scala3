abstract class A(x: Int)

abstract class B(y: Int) extends A({ def f(x: Int) = x * x; f(y)})
