package scala.tasty.statement

import scala.tasty._

trait Definition extends Statement {
  def name: Name
  def owner: Definition
}
