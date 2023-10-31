trait MatchResult[+T]

@main def Test() = {
  def convert[T <: Seq[?], U <: MatchResult[?]](fn: T => U)(implicit x: Seq[?] = Seq.empty): U = ???
  def resultOf[T](v: T): MatchResult[T] = ???

  convert { _ =>
    type R = String
    resultOf[R](???)
    // this would not lead to crash:
    // val x = resultOf[R](???)
    // x
  }
}