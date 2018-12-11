package scala.tasty.file

import scala.tasty.Reflection

trait TastyConsumer {
  def apply(reflect: Reflection)(root: reflect.Tree): Unit
}
