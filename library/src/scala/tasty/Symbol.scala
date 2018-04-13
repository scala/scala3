package scala.tasty

trait Symbol {
  def name: String // TODO should be Name?
  def owner: Symbol
  def definition: Option[statements.Definition]
}
