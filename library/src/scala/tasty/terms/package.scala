package scala.tasty

package object terms {

  object Ident {
    type Data = (names.TermName)
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyIdent(arg)
  }

  object Select {
    type Data = (Term, names.PossiblySignedName)
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplySelect(arg)
  }

  object Literal {
    type Data = constants.Constant
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyLiteral(arg)
  }

  object This {
    type Data = Option[Id]
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyThis(arg)
  }

  object New {
    type Data = typetrees.TypeTree
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyNew(arg)
  }

  object NamedArg {
    type Data = (names.TermName, Term)
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyNamedArg(arg)
  }

  object Apply {
    type Data = (Term, List[Term])
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyApply(arg)
  }

  object TypeApply {
    type Data = (Term, List[typetrees.TypeTree])
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyTypeApply(arg)
  }

  object Super {
    type Data = (Term, Option[Id])
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplySuper(arg)
  }

  object Typed {
    type Data = (Term, typetrees.TypeTree)
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyTyped(arg)
  }

  object Assign {
    type Data = (Term, Term)
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyAssign(arg)
  }

  object Block {
    type Data = (List[statements.Statement], Term)
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyBlock(arg)
  }

  object Inlined {
    type Data = (Term, List[statements.Definition], Term)
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyInlined(arg)
  }

  object Lambda {
    type Data = (Term, Option[typetrees.TypeTree])
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyLambda(arg)
  }

  object If {
    type Data = (Term, Term, Term)
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyIf(arg)
  }

  object Match {
    type Data = (Term, List[patterns.CaseDef])
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyMatch(arg)
  }

  object Try {
    type Data = (Term, List[patterns.CaseDef], Option[Term])
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyTry(arg)
  }

  object Return {
    type Data = Term
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyReturn(arg)
  }

  object Repeated {
    type Data = List[Term]
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyRepeated(arg)
  }

  object SelectOuter {
    type Data = (Term, Int, types.Type)
    def unapply(arg: statements.TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplySelectOuter(arg)
  }

}
