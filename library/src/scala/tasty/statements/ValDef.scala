package scala.tasty
package statements

import scala.tasty.Extractor
import scala.tasty.modifiers.Modifier

trait ValDef extends Definition

object ValDef {
  type Data = (names.TermName, typetrees.TypeTree, Option[terms.Term], List[Modifier])
  def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyValDef(arg)
}
