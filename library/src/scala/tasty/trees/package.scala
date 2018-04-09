package scala.tasty

import scala.runtime.tasty.Toolbox

package object trees {

  // Term trees

  object Ident {
    type Data = (names.TermName)
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyIdent(arg)
  }

  object Select {
    type Data = (Term, names.PossiblySignedName)
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplySelect(arg)
  }

  object Literal {
    type Data = constants.Constant
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyLiteral(arg)
  }

  object This {
    type Data = Option[Id]
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyThis(arg)
  }

  object New {
    type Data = TypeTree
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyNew(arg)
  }

  object NamedArg {
    type Data = (names.TermName, Term)
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyNamedArg(arg)
  }

  object Apply {
    type Data = (Term, List[Term])
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyApply(arg)
  }

  object TypeApply {
    type Data = (Term, List[TypeTree])
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyTypeApply(arg)
  }

  object Super {
    type Data = (Term, Option[Id])
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplySuper(arg)
  }

  object Typed {
    type Data = (Term, TypeTree)
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyTyped(arg)
  }

  object Assign {
    type Data = (Term, Term)
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyAssign(arg)
  }

  object Block {
    type Data = (List[Statement], Term)
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyBlock(arg)
  }

  object Inlined {
    type Data = (Term, List[Definition], Term)
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyInlined(arg)
  }

  object Lambda {
    type Data = (Term, Option[TypeTree])
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyLambda(arg)
  }

  object If {
    type Data = (Term, Term, Term)
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyIf(arg)
  }

  object Match {
    type Data = (Term, List[CaseDef])
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyMatch(arg)
  }

  object Try {
    type Data = (Term, List[CaseDef], Option[Term])
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyTry(arg)
  }

  object Return {
    type Data = Term
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyReturn(arg)
  }

  object Repeated {
    type Data = List[Term]
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyRepeated(arg)
  }

  object SelectOuter {
    type Data = (Term, Int, types.Type)
    def unapply(arg: Term)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplySelectOuter(arg)
  }

  // Type trees

  object Synthetic {
    def unapply(arg: TypeTree)(implicit ctx: Context): Boolean = ctx.toolbox.unapplySynthetic(arg)
  }

  object TypeIdent {
    type Data = names.TypeName
    def unapply(arg: TypeTree)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyTypeIdent(arg)
  }

  object TypeSelect {
    type Data = (Term, names.TypeName)
    def unapply(arg: TypeTree)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyTypeSelect(arg)
  }

  object Singleton {
    type Data = Term
    def unapply(arg: TypeTree)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplySingleton(arg)
  }

  object Refined {
    type Data = (TypeTree, List[Definition])
    def unapply(arg: TypeTree)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyRefined(arg)
  }

  object Applied {
    type Data = (TypeTree, List[TypeTree])
    def unapply(arg: TypeTree)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyApplied(arg)
  }

  object Annotated {
    type Data = (TypeTree, Term)
    def unapply(arg: TypeTree)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyAnnotated(arg)
  }

  object And {
    type Data = (TypeTree, TypeTree)
    def unapply(arg: TypeTree)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyAnd(arg)
  }

  object Or {
    type Data = (TypeTree, TypeTree)
    def unapply(arg: TypeTree)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyOr(arg)
  }

  object ByName {
    type Data = TypeTree
    def unapply(arg: TypeTree)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyByName(arg)
  }

}
