import language.experimental.captureChecking
import scala.annotation.unchecked.uncheckedVariance

trait IterableFactory[+CC[_]] extends caps.Pure:

  def fill[A](n: Int)(elem: => A): CC[A]^{elem} = ???
  def fill[A](n1: Int, n2: Int)(elem: => A): CC[(CC[A]^{elem}) @uncheckedVariance]^{elem} =
  	fill[CC[A]^{elem}](n1)(fill(n2)(elem))
  	fill(n1)(fill(n2)(elem))

  def fill2[A](n: Int)(elem: () => A): CC[A]^{elem} = ???
  def fill2[A](n1: Int, n2: Int)(elem: () => A): CC[(CC[A]^{elem}) @uncheckedVariance]^{elem} =
  	fill2[CC[A]^{elem}](n1)(() => fill2(n2)(elem))
  	fill2(n1)(() => fill2(n2)(elem))




