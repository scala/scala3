package scala.tasty

import scala.tasty.trees.Definition

trait Symbol {
  def name: String // TODO should be Name?
  def owner: Symbol
  def definition: Option[Definition]
}
