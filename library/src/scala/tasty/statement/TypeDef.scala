package scala.tasty.statement

import scala.tasty._
import scala.tasty.typetree.TypeTree

trait TypeDef extends Definition

object TypeDef {
  type Data = (TypeName, TypeTree, List[Modifier])
  def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyTypeDef(arg)
}
