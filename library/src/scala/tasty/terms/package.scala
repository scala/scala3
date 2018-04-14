package scala.tasty

import scala.runtime.tasty.Toolbox

package object terms {

  object Ident {
    type Data = (names.TermName)
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyIdent(arg)
  }

  object Select {
    type Data = (Term, names.PossiblySignedName)
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplySelect(arg)
  }

  object Literal {
    type Data = constants.Constant
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyLiteral(arg)
  }

  object This {
    type Data = Option[Id]
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyThis(arg)
  }

  object New {
    type Data = typetrees.TypeTree
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyNew(arg)
  }

  object NamedArg {
    type Data = (names.TermName, Term)
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyNamedArg(arg)
  }

  object Apply {
    type Data = (Term, List[Term])
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyApply(arg)
  }

  object TypeApply {
    type Data = (Term, List[typetrees.TypeTree])
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyTypeApply(arg)
  }

  object Super {
    type Data = (Term, Option[Id])
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplySuper(arg)
  }

  object Typed {
    type Data = (Term, typetrees.TypeTree)
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyTyped(arg)
  }

  object Assign {
    type Data = (Term, Term)
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyAssign(arg)
  }

  object Block {
    type Data = (List[statements.Statement], Term)
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyBlock(arg)
  }

  object Inlined {
    type Data = (Term, List[statements.Definition], Term)
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyInlined(arg)
  }

  object Lambda {
    type Data = (Term, Option[typetrees.TypeTree])
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyLambda(arg)
  }

  object If {
    type Data = (Term, Term, Term)
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyIf(arg)
  }

  object Match {
    type Data = (Term, List[patterns.CaseDef])
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyMatch(arg)
  }

  object Try {
    type Data = (Term, List[patterns.CaseDef], Option[Term])
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyTry(arg)
  }

  object Return {
    type Data = Term
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyReturn(arg)
  }

  object Repeated {
    type Data = List[Term]
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyRepeated(arg)
  }

  object SelectOuter {
    type Data = (Term, Int, types.Type)
    def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplySelectOuter(arg)
  }

}
