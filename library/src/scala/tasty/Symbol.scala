package scala.tasty

import scala.tasty.names.Name
import scala.tasty.trees.Definition

trait Symbol {
  def name: Name
  def owner: Symbol
  def definition: Option[Definition]
}
