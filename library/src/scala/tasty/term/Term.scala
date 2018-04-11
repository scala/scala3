package scala.tasty.term

import scala.tasty.Type
import scala.tasty.statement.Statement

trait Term extends Statement {
  def tpe: Type
}


