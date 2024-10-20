class A
object Test:
  type F[X] <: Any = X match
    case A => Int

  def foo[T](x: String): T = ???
  def foo[U](x: U): F[U] = ???

  val x1 = foo(A())
  val y: Int = x1

  val x2: Int = foo(A()) // error
