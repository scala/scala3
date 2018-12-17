trait A
object test1 {
  def foo[E](f: (a: A) => (a.type, E)): E = {
    val a = new A {}
    f(a)._2
  }
   val res = foo { a => (a, 42) }
  val x: Int = res
}
 object test2 {
  trait F[A, -E]
  def empty[A](value: A): F[A, Any] = ???
   def hof[R](f: (p: AnyRef) => F[R, p.type]): F[R, Any] = ???
   hof { p =>
    empty(42)
  }
}