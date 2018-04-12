package scala.tasty.terms

import scala.tasty.statements.Statement
import scala.tasty.types.Type

trait Term extends Statement {
  def tpe: Type
}


