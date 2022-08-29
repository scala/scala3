trait MatchResult[+T]

@main def Test() = {
  def convert[T <: Seq[_], U <: MatchResult[_]](fn: T => U)(implicit x: Seq[_] = Seq.empty): U = ???
  def resultOf[T](v: T): MatchResult[T] = ???

  convert { _ =>
    type R = String
    resultOf[R](???)
    // this would not lead to crash:
    // val x = resultOf[R](???)
    // x
  }
}