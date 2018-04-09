package scala.tasty

trait TopLevelStatement extends Positioned

trait Statement extends TopLevelStatement

trait Term extends Statement {
  def tpe: Type
}
