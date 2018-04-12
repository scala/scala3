package scala.tasty
package statements

import scala.tasty.Extractor
import scala.tasty.modifiers.Modifier

trait DefDef extends Definition

object DefDef {
  type Data = (names.TermName, List[TypeDef],  List[List[ValDef]], typetrees.TypeTree, Option[terms.Term], List[Modifier])
  def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyDefDef(arg)
}
