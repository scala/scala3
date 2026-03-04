object test {
  class Seq[T]
  inline def f[T]: T = scala.compiletime.summonFrom {
    case given (Seq[t] =:= T) => new Seq[t]
  }

  f[Seq[Int]]
}
