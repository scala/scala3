package scala.tasty.trees

trait Definition extends Statement {
  def owner: Definition
}
