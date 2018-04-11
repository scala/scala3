package scala.tasty

import scala.tasty.pattern.CaseDef
import scala.tasty.statement.{Statement, TopLevelStatement}
import scala.tasty.typetree.TypeTree

package object term {

  object Ident {
    type Data = (TermName)
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyIdent(arg)
  }

  object Select {
    type Data = (Term, PossiblySignedName)
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplySelect(arg)
  }

  object Literal {
    type Data = Constant
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyLiteral(arg)
  }

  object This {
    type Data = Option[Id]
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyThis(arg)
  }

  object New {
    type Data = TypeTree
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyNew(arg)
  }

  object NamedArg {
    type Data = (TermName, Term)
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyNamedArg(arg)
  }

  object Apply {
    type Data = (Term, List[Term])
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyApply(arg)
  }

  object TypeApply {
    type Data = (Term, List[Term])
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyTypeApply(arg)
  }

  object Super {
    type Data = (Term, Option[Id])
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplySuper(arg)
  }

  object Typed {
    type Data = (Term, TypeTree)
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyTyped(arg)
  }

  object Assign {
    type Data = (Term, Term)
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyAssign(arg)
  }

  object Block {
    type Data = (List[Statement], Term)
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyBlock(arg)
  }

  //  case Inlined(call: Term, bindings: List[Definition], expr: Term)

  object Lambda {
    type Data = (Term, Option[TypeTree])
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyLambda(arg)
  }

  object If {
    type Data = (Term, Term, Term)
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyIf(arg)
  }

  object Match {
    type Data = (Term, List[CaseDef])
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyMatch(arg)
  }

  object Try {
    type Data = (Term, List[CaseDef], Option[Term])
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyTry(arg)
  }

  object Return {
    type Data = Term
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyReturn(arg)
  }

  object Repeated {
    type Data = List[Term]
    def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyRepeated(arg)
  }

}
