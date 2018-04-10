package scala.tasty

trait Definition extends Statement {
  def name: Name
  def owner: Definition
}
