@main def test: Unit = {
  trait TyCon[+A]
  trait S[T]
  trait P[T] extends S[TyCon[T]] {
    def consume(t: T): Unit
  }

  def patmat(s: S[TyCon[Int]]) = s match {
    case p: P[t] =>
      p.consume("Hi") // error
  }

  patmat(new P[Int] {
    override def consume(t: Int): Unit = t + 1
  })
}
