package scala.tasty
package statements

import scala.tasty.Extractor
import scala.tasty.modifiers.Modifier

trait TypeDef extends Definition

object TypeDef {
  type Data = (names.TypeName, typetrees.TypeTree, List[Modifier])
  def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyTypeDef(arg)
}
