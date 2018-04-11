package scala.tasty

trait Definition extends Statement {
  def name: Name
  def owner: Definition
}

trait ValDef extends Definition

trait DefDef extends Definition

trait TypeDef extends Definition

trait ClassDef extends Definition
