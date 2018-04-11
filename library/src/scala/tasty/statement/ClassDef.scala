package scala.tasty.statement

import scala.tasty._
import scala.tasty.term.Term

trait ClassDef extends Definition

object ClassDef {
  type Data = (TypeName, DefDef, List[Term],  Option[ValDef], List[Statement], List[Modifier])
  def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyClassDef(arg)
}
