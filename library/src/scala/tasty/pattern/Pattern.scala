package scala.tasty.pattern

import scala.tasty.{Positioned, Type}

trait Pattern extends Positioned {
  def tpe: Type
}
