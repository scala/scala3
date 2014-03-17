trait Empty[E[_]] {
  def e[A]: E[A]
}

object T {
  val ListEmpty = new Empty[List] {
    def e[A]/*: List*/ = Nil  // uncomment to get crash
  }

  def foo[F[_]](q:(String,String)) = "hello"
  def foo[F[_]](e: Empty[F]) = "world"

  val x = foo[List](ListEmpty)
}
