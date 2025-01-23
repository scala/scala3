package scala // @preview in private[scala]

import scala.annotation.internal.preview

trait A:
  def f: Int
  def g: Int = 3
trait B extends A:
  @preview
  def f: Int = 4 // error

  @preview
  override def g: Int = 5 // error
