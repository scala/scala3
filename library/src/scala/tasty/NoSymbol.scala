package scala.tasty

object NoSymbol extends Symbol {
  def name: String = "NoSymbol"
  def owner: Symbol = NoSymbol
  def definition: None.type = None
  override def toString: String = "NoSymbol"
}
