package scala.tasty.trees

import scala.tasty.types.Type

trait Term extends Statement {
  def tpe: Type
}


