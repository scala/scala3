package scala.tasty.file

import scala.tasty.Tasty

trait TastyConsumer {
  def apply(tasty: Tasty)(root: tasty.Tree): Unit
}
