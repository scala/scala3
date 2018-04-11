package scala.tasty.statement

import scala.tasty._
import scala.tasty.term.Term
import scala.tasty.typetree.TypeTree

trait ValDef extends Definition

object ValDef {
  type Data = (TermName, TypeTree, Option[Term], List[Modifier])
  def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyValDef(arg)
}
