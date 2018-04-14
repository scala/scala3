package scala.tasty.trees

import scala.tasty.Symbol

trait Definition extends Statement {
  def sym: Symbol
}
