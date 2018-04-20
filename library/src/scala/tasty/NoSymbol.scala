package scala.tasty

import scala.tasty.names.Name

object NoSymbol extends Symbol {
  def name: Name = new Name { override def toString = "<NoSymbol>" } // name that cannot be extracted
  def owner: Symbol = NoSymbol
  def definition: None.type = None
  override def toString: String = "NoSymbol"
}
