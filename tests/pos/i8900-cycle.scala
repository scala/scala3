trait Contra[-A]
trait Cov[+B]

trait Test {
  def foo[S](x: S): S
  def rec1[T <: Cov[T]]: Contra[T]
  def rec2[T <: Cov[U], U <: T]: Contra[T]

  val a = foo({
    rec1
  })
  val b = foo({
    rec2
  })
}
