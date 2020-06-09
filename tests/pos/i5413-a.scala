trait Or[A]
trait C

object ImplicitChainTest {
  def ipl[A](implicit from: A => Or[A]): C = null
  ipl
}
