package scala.tasty.statement

import scala.tasty._
import scala.tasty.term.Term
import scala.tasty.typetree.TypeTree

trait DefDef extends Definition

object DefDef {
  type Data = (TermName, List[TypeDef],  List[List[ValDef]], TypeTree, Option[Term], List[Modifier])
  def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyDefDef(arg)
}
