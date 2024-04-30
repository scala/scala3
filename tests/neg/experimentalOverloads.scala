

import scala.annotation.experimental

trait A:
  def f: Int
  def g: Int = 3
trait B extends A:
  @experimental
  def f: Int = 4 // error

  @experimental
  override def g: Int = 5 // error
