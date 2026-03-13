package scala.quoted.compiletime.internal

import dotty.tools.dotc
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.NameKinds
import scala.quoted.Expr
import scala.quoted.Type
import scala.quoted.compiletime as pub

/////// Tree ///////////////////////////////////////////////////////////////

type Tree = pub.Tree & TreeImpl

sealed trait TreeImpl { _self: pub.Tree =>

  val underlying: tpd.Tree

  override final def toString: String = underlying.toString
  override lazy val hashCode: Int = underlying.hashCode
  override final def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: TreeImpl => this.underlying == that.underlying
    case _              => false

  override def pos: Position = PositionImpl(underlying.sourcePos)

  override def symbol: Symbol = SymbolImpl(underlying.symbol)

  override def show(using pub.Printer[pub.Tree]): String = ???

  override def isExpr: Boolean = ???

  override def asExpr: Expr[Any] = ???

  override def asExprOf[T](using Type[T]): Expr[T] = ???

  override def changeOwner(newOwner: pub.Symbol): Tree = TreeImpl(tpd.TreeOps(underlying).changeNonLocalOwners(newOwner.cast.symbol))

}

object TreeImpl {

  implicit def treeToDotc(tree: pub.Tree): tpd.Tree = tree.cast.underlying

  private[internal] def optional[T <: tpd.Tree](x: T): Option[T] =

    if x.isEmpty then None else Some(x)

  private def effectivePatterns(patterns: List[tpd.Tree]): List[tpd.Tree] =

    patterns match

      case patterns0 :+ dotty.tools.dotc.ast.Trees.SeqLiteral(elems, _) => patterns0 ::: elems

      case _ => patterns

  def apply(tree: tpd.Tree): TreeImpl = tree match {

    case tree: tpd.PackageDef => new PackageClauseImpl(tree)

    case tree: tpd.Import => new ImportImpl(tree)

    case tree: tpd.Export => new ExportImpl(tree)

    case tree: tpd.TypeDef =>

      if tree.isClassDef then new ClassDefImpl(tree)
      else new TypeDefImpl(tree)

    case tree: tpd.DefDef => new DefDefImpl(tree)

    case tree: tpd.ValDef => new ValDefImpl(tree)

    case tree: tpd.Ident =>

      if tree.isTerm then

        if tree.name == nme.WILDCARD then new WildcardImpl(tree)
        else new IdentImpl(tree)
      else if tree.name == nme.WILDCARD then new WildcardTypeTreeImpl(tree)
      else new TypeIdentImpl(tree)

    case tree: tpd.Select =>

      if tree.isTerm then

        tree.name match

          case NameKinds.OuterSelectName(_, _) => new SelectOuterImpl(tree)

          case _ => new SelectImpl(tree)
      else if tree.qualifier.isTerm then new TypeSelectImpl(tree)
      else new TypeProjectionImpl(tree)

    case tree: tpd.Literal => new LiteralImpl(tree)

    case tree: tpd.This => new ThisImpl(tree)

    case tree: tpd.New => new NewImpl(tree)

    case tree: tpd.NamedArg => new NamedArgImpl(tree)

    case tree: tpd.Apply => new ApplyImpl(tree)

    case tree: tpd.TypeApply => new TypeApplyImpl(tree)

    case tree: tpd.Super => new SuperImpl(tree)

    case tree: tpd.Typed => new TypedImpl(tree)

    case tree: tpd.Assign => new AssignImpl(tree)

    case tree: tpd.Block =>

      if tree.isTerm then new BlockImpl(tree)
      else new TypeBlockImpl(tree)

    case tree: tpd.Closure => new ClosureImpl(tree)

    case tree: tpd.If => new IfImpl(tree)

    case tree: tpd.Match =>

      if !tree.selector.isEmpty then new MatchImpl(tree)
      else new SummonFromImpl(tree)

    case tree: tpd.Try => new TryImpl(tree)

    case tree: tpd.Return => new ReturnImpl(tree)

    case tree: tpd.SeqLiteral => new RepeatedImpl(tree)

    case tree: tpd.Inlined => new InlinedImpl(tree)

    case tree: tpd.WhileDo => new WhileImpl(tree)

    case tree: tpd.TypeTree =>

      if tree.tpe.isInstanceOf[dotc.core.Types.TypeBounds] then new TypeBoundsTreeImpl(tree)
      else new InferredImpl(tree)

    case tree: tpd.SingletonTypeTree => new SingletonImpl(tree)

    case tree: tpd.RefinedTypeTree => new RefinedImpl(tree)

    case tree: tpd.AppliedTypeTree => new AppliedImpl(tree)

    case tree: tpd.Annotated => new AnnotatedImpl(tree)

    case tree: tpd.MatchTypeTree => new MatchTypeTreeImpl(tree)

    case tree: tpd.ByNameTypeTree => new ByNameImpl(tree)

    case tree: tpd.LambdaTypeTree => new LambdaTypeTreeImpl(tree)

    case tree: tpd.Bind =>

      if tree.name.isTypeName then new TypeBindImpl(tree)
      else new BindImpl(tree)

    case tree: tpd.TypeBoundsTree => new TypeBoundsTreeImpl(tree)

    case tree: tpd.CaseDef =>

      if tree.body.isTerm then new CaseDefImpl(tree)
      else new TypeCaseDefImpl(tree)

    case tree: tpd.UnApply => new UnapplyImpl(tree)

    case tree: tpd.QuotePattern => new UnapplyImpl(tree)

    case tree: tpd.Alternative => new AlternativesImpl(tree)

    case tree: tpd.Quote => new ApplyImpl(tree)

    case tree: tpd.Splice => new ApplyImpl(tree)

    case _ => throw new MatchError(tree)

  }

  object Module extends pub.Tree.Module {}

}

/////// PackageClause ///////////////////////////////////////////////////////////////

type PackageClause = PackageClauseImpl

final class PackageClauseImpl(val underlying: tpd.PackageDef) extends TreeImpl, pub.PackageClause {

  override def pid: Ref = TreeImpl(underlying.pid).asInstanceOf[Ref]

  override def stats: List[Tree] = underlying.stats.map(TreeImpl(_))

}

object PackageClauseImpl {

  object Module extends pub.PackageClause.Module {

    override def apply(pid: pub.Ref, stats: List[pub.Tree]): pub.PackageClause = ???

    override def make(pid: pub.Ref, stats: List[pub.Tree]): pub.PackageClause = ???

  }

}

/////// Statement ///////////////////////////////////////////////////////////////

type Statement = pub.Statement & StatementImpl

sealed trait StatementImpl extends TreeImpl { _self: pub.Statement => }

object StatementImpl {

  object Module extends pub.Statement.Module {}

}

/////// Import ///////////////////////////////////////////////////////////////

type Import = ImportImpl

final class ImportImpl(val underlying: tpd.Import) extends StatementImpl, pub.Import {

  override def expr: Term = TreeImpl(underlying.expr).asInstanceOf[Term]

  override def selectors: List[Selector] = underlying.selectors.map(SelectorImpl(_))

}

object ImportImpl {

  object Module extends pub.Import.Module {

    override def apply(expr: pub.Term, selectors: List[pub.Selector]): pub.Import = ???

    override def make(expr: pub.Term, selectors: List[pub.Selector]): pub.Import = ???

  }

}

/////// Export ///////////////////////////////////////////////////////////////

type Export = ExportImpl

final class ExportImpl(val underlying: tpd.Export) extends StatementImpl, pub.Export {

  override def expr: Term = TreeImpl(underlying.expr).asInstanceOf[Term]

  override def selectors: List[Selector] = underlying.selectors.map(SelectorImpl(_))

}

object ExportImpl {

  object Module extends pub.Export.Module {}

}

/////// Definition ///////////////////////////////////////////////////////////////

type Definition = pub.Definition & DefinitionImpl

sealed trait DefinitionImpl extends StatementImpl { _self: pub.Definition =>

  override val underlying: tpd.MemberDef

  override def name: String = underlying.name.toString

}

object DefinitionImpl {

  object Module extends pub.Definition.Module {}

}

/////// ClassDef ///////////////////////////////////////////////////////////////

type ClassDef = ClassDefImpl

final class ClassDefImpl(override val underlying: tpd.TypeDef) extends DefinitionImpl, pub.ClassDef {

  override def constructor: DefDef = TreeImpl(underlying.rhs.asInstanceOf[tpd.Template].constr).asInstanceOf[DefDef]

  override def parents: List[Tree] = underlying.rhs.asInstanceOf[tpd.Template].parents.map(TreeImpl(_))

  override def self: Option[ValDef] = TreeImpl.optional(underlying.rhs.asInstanceOf[tpd.Template].self).map(TreeImpl(_).asInstanceOf[ValDef])

  override def body: List[Statement] = underlying.rhs.asInstanceOf[tpd.Template].body.map(TreeImpl(_).asInstanceOf[Statement])

}

object ClassDefImpl {

  object Module extends pub.ClassDef.Module {

    override def apply(cls: pub.Symbol, parents: List[pub.Tree], body: List[pub.Statement]): pub.ClassDef = ???

    override def make(cls: pub.Symbol, parents: List[pub.Tree], body: List[pub.Statement]): pub.ClassDef = ???

    override def module(module: pub.Symbol, parents: List[pub.Tree], body: List[pub.Statement]): (pub.ValDef, pub.ClassDef) = ???

  }

}

/////// ValOrDefDef ///////////////////////////////////////////////////////////////

type ValOrDefDef = pub.ValOrDefDef & ValOrDefDefImpl

sealed trait ValOrDefDefImpl extends DefinitionImpl { _self: pub.ValOrDefDef =>

  override val underlying: tpd.ValOrDefDef

  override def tpt: TypeTree = TreeImpl(underlying.tpt).asInstanceOf[TypeTree]

  override def rhs: Option[Term] = TreeImpl.optional(underlying.rhs).map(TreeImpl(_).asInstanceOf[Term])

}

object ValOrDefDefImpl {

  object Module extends pub.ValOrDefDef.Module {}

}

/////// DefDef ///////////////////////////////////////////////////////////////

type DefDef = DefDefImpl

final class DefDefImpl(override val underlying: tpd.DefDef) extends ValOrDefDefImpl, pub.DefDef {

  override def paramss: List[ParamClause] = underlying.paramss.map(ParamClauseImpl(_))

  override def leadingTypeParams: List[TypeDef] = underlying.leadingTypeParams.map(TreeImpl(_).asInstanceOf[TypeDef])

  override def trailingParamss: List[ParamClause] = underlying.trailingParamss.map(ParamClauseImpl(_))

  override def termParamss: List[TermParamClause] = underlying.termParamss.map(TermParamClauseImpl(_))

  override def returnTpt: TypeTree = TreeImpl(underlying.tpt).asInstanceOf[TypeTree]

}

object DefDefImpl {

  object Module extends pub.DefDef.Module {

    override def apply(symbol: pub.Symbol, rhsFn: List[List[pub.Tree]] => Option[pub.Term]): pub.DefDef = ???

    override def make(symbol: pub.Symbol, rhsFn: List[List[pub.Tree]] => Option[pub.Term]): pub.DefDef = ???

  }

}

/////// ValDef ///////////////////////////////////////////////////////////////

type ValDef = ValDefImpl

final class ValDefImpl(override val underlying: tpd.ValDef) extends ValOrDefDefImpl, pub.ValDef

object ValDefImpl {

  object Module extends pub.ValDef.Module {

    override def apply(symbol: pub.Symbol, rhs: Option[pub.Term]): pub.ValDef = ???

    override def make(symbol: pub.Symbol, rhs: Option[pub.Term]): pub.ValDef = ???

    override def let(owner: pub.Symbol, name: String, rhs: pub.Term, flags: pub.Flags)(body: pub.Ref => pub.Term): pub.Term = ???

    override def let(owner: pub.Symbol, name: String, rhs: pub.Term)(body: pub.Ref => pub.Term): pub.Term = ???

    override def let(owner: pub.Symbol, rhs: pub.Term)(body: pub.Ref => pub.Term): pub.Term = ???

    override def let(owner: pub.Symbol, terms: List[pub.Term])(body: List[pub.Ref] => pub.Term): pub.Term = ???

  }

}

/////// TypeDef ///////////////////////////////////////////////////////////////

type TypeDef = TypeDefImpl

final class TypeDefImpl(override val underlying: tpd.TypeDef) extends DefinitionImpl, pub.TypeDef {

  override def rhs: Tree = TreeImpl(underlying.rhs)

}

object TypeDefImpl {
  object Module extends pub.TypeDef.Module {
    override def apply(symbol: pub.Symbol): pub.TypeDef = ???
    override def make(symbol: pub.Symbol): pub.TypeDef = ???
  }
}

/////// Term ///////////////////////////////////////////////////////////////

type Term = pub.Term & TermImpl
sealed trait TermImpl extends StatementImpl { _self: pub.Term =>
  override def tpe: TypeRepr = TypeReprImpl(underlying.tpe.widenSkolem)
  override def underlyingArgument: Term = TreeImpl(new tpd.TreeOps(underlying).underlyingArgument).asInstanceOf[Term]
  override def underlying: Term = TreeImpl(new tpd.TreeOps(underlying).underlying).asInstanceOf[Term]
  override def etaExpand(owner: pub.Symbol): Term =
    val t = underlying.tpe.widen match {
      case mtpe: dotc.core.Types.MethodType if !mtpe.isParamDependent =>
        val closureResType = mtpe.resType match {
          case t: dotc.core.Types.MethodType => t.toFunctionType(isJava = underlying.symbol.is(dotc.core.Flags.JavaDefined))
          case t                             => t
        }
        val closureTpe = dotc.core.Types.MethodType(mtpe.paramNames, mtpe.paramInfos, closureResType)
        val closureMethod = dotc.core.Symbols.newAnonFun(owner.cast.symbol, closureTpe)
        tpd.Closure(closureMethod, tss => new tpd.TreeOps(underlying).appliedToTermArgs(tss.head).etaExpand(closureMethod))
      case _ => underlying
    }
    TreeImpl(t).asInstanceOf[Term]

  override def appliedTo(arg: pub.Term): Term = appliedToArgs(List(arg.cast))
  override def appliedTo(arg: pub.Term, args: pub.Term*): Term = appliedToArgs(arg.cast :: args.map(_.cast).toList)
  override def appliedToArgs(args: List[pub.Term]): Apply = ApplyImpl(tpd.Apply(underlying, args.map(_.cast.underlying)))
  override def appliedToArgss(argss: List[List[pub.Term]]): Term =
    TreeImpl(argss.foldLeft(underlying)((acc, args) => tpd.Apply(acc, args.map(_.cast.underlying)))).asInstanceOf[Term]
  override def appliedToNone: Apply = appliedToArgs(Nil)
  override def ensureApplied: Term =
    def isParameterless(tpe: TypeRepr): Boolean = !tpe.isInstanceOf[MethodType]
    if isParameterless(tpe.widen) then this.asInstanceOf[Term] else appliedToNone.asInstanceOf[Term]
  override def appliedToType(targ: pub.TypeRepr): Term = appliedToTypes(List(targ.cast))
  override def appliedToTypes(targs: List[pub.TypeRepr]): Term = appliedToTypeTrees(targs.map(t => InferredImpl(tpd.TypeTree(t.cast.underlying))))
  override def appliedToTypeTrees(targs: List[pub.TypeTree]): Term =
    if targs.isEmpty then this.asInstanceOf[Term] else TreeImpl(tpd.TypeApply(underlying, targs.map(_.cast.underlying))).asInstanceOf[Term]
  override def select(sym: pub.Symbol): Select = SelectImpl(tpd.Select(underlying, sym.cast.symbol))
}
object TermImpl {
  object Module extends pub.Term.Module {
    override def betaReduce(term: pub.Term): Option[pub.Term] = ???
  }
}

/////// Ref ///////////////////////////////////////////////////////////////

type Ref = pub.Ref & RefImpl
sealed trait RefImpl extends TermImpl { _self: pub.Ref =>
  override val underlying: tpd.RefTree
}
object RefImpl {
  object Module extends pub.Ref.Module {
    override def term(tp: pub.TermRef): pub.Ref = ???
    override def apply(sym: pub.Symbol): pub.Ref = ???
    override def make(sym: pub.Symbol): pub.Ref = ???
  }
}

/////// Ident ///////////////////////////////////////////////////////////////

type Ident = IdentImpl
final class IdentImpl(override val underlying: tpd.Ident) extends RefImpl, pub.Ident {
  override def name: String = underlying.name.toString
}
object IdentImpl {
  object Module extends pub.Ident.Module {
    override def apply(tmref: pub.TermRef): pub.Term = ???
    override def make(tmref: pub.TermRef): pub.Term = ???
  }
}

/////// Wildcard ///////////////////////////////////////////////////////////////

type Wildcard = WildcardImpl
final class WildcardImpl(override val underlying: tpd.Ident) extends RefImpl, pub.Wildcard {
  override def name: String = underlying.name.toString
}
object WildcardImpl {
  object Module extends pub.Wildcard.Module {
    override def apply(): pub.Wildcard = ???
    override def make(): pub.Wildcard = ???
  }
}

/////// Select ///////////////////////////////////////////////////////////////

type Select = SelectImpl
final class SelectImpl(override val underlying: tpd.Select) extends RefImpl, pub.Select {
  override def qualifier: Term = TreeImpl(underlying.qualifier).asInstanceOf[Term]
  override def name: String = underlying.name.toString
  override def signature: Option[Signature] =
    if underlying.symbol.signature == dotc.core.Signature.NotAMethod then None
    else Some(SignatureImpl(underlying.symbol.signature))
}
object SelectImpl {
  object Module extends pub.Select.Module {
    override def apply(qualifier: pub.Term, symbol: pub.Symbol): pub.Select = ???
    override def make(qualifier: pub.Term, symbol: pub.Symbol): pub.Select = ???
    override def unique(qualifier: pub.Term, name: String): pub.Select = ???
    override def overloaded(qualifier: pub.Term, name: String, targs: List[pub.TypeRepr], args: List[pub.Term]): pub.Term = ???
    override def overloaded(qualifier: pub.Term, name: String, targs: List[pub.TypeRepr], args: List[pub.Term], returnType: pub.TypeRepr): pub.Term = ???
  }
}

/////// Literal ///////////////////////////////////////////////////////////////

type Literal = LiteralImpl
final class LiteralImpl(val underlying: tpd.Literal) extends TermImpl, pub.Literal {
  override def constant: Constant = ConstantImpl(underlying.const)
}
object LiteralImpl {
  object Module extends pub.Literal.Module {
    override def apply(constant: pub.Constant): pub.Literal = ???
    override def make(constant: pub.Constant): pub.Literal = ???
  }
}

/////// This ///////////////////////////////////////////////////////////////

type This = ThisImpl
final class ThisImpl(val underlying: tpd.This) extends TermImpl, pub.This {
  override def id: Option[String] = TreeImpl.optional(underlying.qual).map(_.name.toString)
}
object ThisImpl {
  object Module extends pub.This.Module {
    override def apply(cls: pub.Symbol): pub.This = ???
    override def make(cls: pub.Symbol): pub.This = ???
  }
}

/////// New ///////////////////////////////////////////////////////////////

type New = NewImpl
final class NewImpl(val underlying: tpd.New) extends TermImpl, pub.New {
  override def tpt: TypeTree = TreeImpl(underlying.tpt).asInstanceOf[TypeTree]
}
object NewImpl {
  object Module extends pub.New.Module {
    override def apply(tpt: pub.TypeTree): pub.New = ???
    override def make(tpt: pub.TypeTree): pub.New = ???
  }
}

/////// NamedArg ///////////////////////////////////////////////////////////////

type NamedArg = NamedArgImpl
final class NamedArgImpl(val underlying: tpd.NamedArg) extends TermImpl, pub.NamedArg {
  override def name: String = underlying.name.toString
  override def value: Term = TreeImpl(underlying.arg).asInstanceOf[Term]
}
object NamedArgImpl {
  object Module extends pub.NamedArg.Module {
    override def apply(name: String, arg: pub.Term): pub.NamedArg = ???
    override def make(name: String, arg: pub.Term): pub.NamedArg = ???
  }
}

/////// Apply ///////////////////////////////////////////////////////////////

type Apply = ApplyImpl
final class ApplyImpl(val underlying: tpd.Tree) extends TermImpl, pub.Apply {
  override def fun: Term =
    val t = underlying match
      case self: tpd.Apply => self.fun
      case self: tpd.Quote =>
        tpd
          .ref(dotc.core.Symbols.defn.QuotedRuntime_exprQuote)
          .appliedToType(self.bodyType)
          .withSpan(self.span)
      case self: tpd.Splice =>
        tpd
          .ref(dotc.core.Symbols.defn.QuotedRuntime_exprSplice)
          .appliedToType(self.tpe)
          .withSpan(self.span)
    TreeImpl(t).asInstanceOf[Term]

  override def args: List[Term] =
    val ts = underlying match
      case self: tpd.Apply  => self.args
      case self: tpd.Quote  => List(self.body)
      case self: tpd.Splice => List(self.expr)
    ts.map(TreeImpl(_).asInstanceOf[Term])
}
object ApplyImpl {
  object Module extends pub.Apply.Module {
    override def apply(fun: pub.Term, args: List[pub.Term]): pub.Apply = ???
    override def make(fun: pub.Term, args: List[pub.Term]): pub.Apply = ???
  }
}

/////// TypeApply ///////////////////////////////////////////////////////////////

type TypeApply = TypeApplyImpl
final class TypeApplyImpl(val underlying: tpd.TypeApply) extends TermImpl, pub.TypeApply {
  override def fun: Term = TreeImpl(underlying.fun).asInstanceOf[Term]
  override def args: List[TypeTree] = underlying.args.map(TreeImpl(_).asInstanceOf[TypeTree])
}
object TypeApplyImpl {
  object Module extends pub.TypeApply.Module {
    override def apply(fun: pub.Term, args: List[pub.TypeTree]): pub.TypeApply = ???
    override def make(fun: pub.Term, args: List[pub.TypeTree]): pub.TypeApply = ???
  }
}

/////// Super ///////////////////////////////////////////////////////////////

type Super = SuperImpl
final class SuperImpl(val underlying: tpd.Super) extends TermImpl, pub.Super {
  override def qualifier: Term = TreeImpl(underlying.qual).asInstanceOf[Term]
  override def id: Option[String] = TreeImpl.optional(underlying.mix).map(_.name.toString)
  override def idPos: Position = PositionImpl(underlying.mix.sourcePos)
}
object SuperImpl {
  object Module extends pub.Super.Module {
    override def apply(qual: pub.Term, mix: Option[String]): pub.Super = ???
    override def make(qual: pub.Term, mix: Option[String]): pub.Super = ???
  }
}

/////// Typed ///////////////////////////////////////////////////////////////

type Typed = TypedImpl
final class TypedImpl(val underlying: tpd.Typed) extends TermImpl, pub.Typed {
  // Typed extends Term AND TypedOrTest.
  // TermImpl is a trait. TypedOrTestImpl is a trait.
  // I need to mix in TypedOrTestImpl as well? No, TermImpl covers TreeImpl.
  // But TypedOrTest adds `tree` method which `Typed` might not have?
  // `Typed` has `expr` which corresponds to `tree` in TypedOrTest.
  // I need to check `Typed` definition in Tree.scala.
  // `trait Typed ... extends Term with TypedOrTest { def expr: Term; def tpt: TypeTree }`
  // `TypedOrTest` has `def tree: Tree; def tpt: TypeTree`.
  // So `Typed` must implement `tree`.
  override def expr: Term = TreeImpl(underlying.expr).asInstanceOf[Term]
  override def tpt: TypeTree = TreeImpl(underlying.tpt).asInstanceOf[TypeTree]
  override def tree: Tree = TreeImpl(underlying.expr) // From TypedOrTest
}
object TypedImpl {
  object Module extends pub.Typed.Module {
    override def apply(expr: pub.Term, tpt: pub.TypeTree): pub.Typed = ???
    override def make(expr: pub.Term, tpt: pub.TypeTree): pub.Typed = ???
  }
}

/////// Assign ///////////////////////////////////////////////////////////////

type Assign = AssignImpl
final class AssignImpl(val underlying: tpd.Assign) extends TermImpl, pub.Assign {
  override def lhs: Term = TreeImpl(underlying.lhs).asInstanceOf[Term]
  override def rhs: Term = TreeImpl(underlying.rhs).asInstanceOf[Term]
}
object AssignImpl {
  object Module extends pub.Assign.Module {
    override def apply(lhs: pub.Term, rhs: pub.Term): pub.Assign = ???
    override def make(lhs: pub.Term, rhs: pub.Term): pub.Assign = ???
  }
}

/////// Block ///////////////////////////////////////////////////////////////

type Block = BlockImpl
final class BlockImpl(val underlying: tpd.Block) extends TermImpl, pub.Block {
  override def statements: List[Statement] = underlying.stats.map(TreeImpl(_).asInstanceOf[Statement])
  override def expr: Term = TreeImpl(underlying.expr).asInstanceOf[Term]
}
object BlockImpl {
  object Module extends pub.Block.Module {
    override def apply(stats: List[pub.Statement], expr: pub.Term): pub.Block = ???
    override def make(stats: List[pub.Statement], expr: pub.Term): pub.Block = ???
  }
}

/////// Closure ///////////////////////////////////////////////////////////////

type Closure = ClosureImpl
final class ClosureImpl(val underlying: tpd.Closure) extends TermImpl, pub.Closure {
  override def meth: Term = TreeImpl(underlying.meth).asInstanceOf[Term]
  override def tpeOpt: Option[TypeRepr] = TreeImpl.optional(underlying.tpt).map(t => TypeReprImpl(t.tpe))
}
object ClosureImpl {
  object Module extends pub.Closure.Module {
    override def apply(meth: pub.Term, tpe: Option[pub.TypeRepr]): pub.Closure = ???
    override def make(meth: pub.Term, tpe: Option[pub.TypeRepr]): pub.Closure = ???
  }
}

/////// Lambda ///////////////////////////////////////////////////////////////

object LambdaImpl {
  object Module extends pub.Lambda.Module {
    override def unapply(tree: pub.Block): Option[(List[pub.ValDef], pub.Term)] = ???
    override def apply(owner: pub.Symbol, tpe: pub.MethodType, rhsFn: (pub.Symbol, List[pub.Tree]) => pub.Tree): pub.Block = ???
    override def make(owner: pub.Symbol, tpe: pub.MethodType, rhsFn: (pub.Symbol, List[pub.Tree]) => pub.Tree): pub.Block = ???
  }
}

/////// If ///////////////////////////////////////////////////////////////

type If = IfImpl
final class IfImpl(val underlying: tpd.If) extends TermImpl, pub.If {
  override def cond: Term = TreeImpl(underlying.cond).asInstanceOf[Term]
  override def thenp: Term = TreeImpl(underlying.thenp).asInstanceOf[Term]
  override def elsep: Term = TreeImpl(underlying.elsep).asInstanceOf[Term]
  override def isInline: Boolean = underlying.isInline
}
object IfImpl {
  object Module extends pub.If.Module {
    override def apply(cond: pub.Term, thenp: pub.Term, elsep: pub.Term): pub.If = ???
    override def make(cond: pub.Term, thenp: pub.Term, elsep: pub.Term): pub.If = ???
  }
}

/////// Match ///////////////////////////////////////////////////////////////

type Match = MatchImpl
final class MatchImpl(val underlying: tpd.Match) extends TermImpl, pub.Match {
  override def scrutinee: Term = TreeImpl(underlying.selector).asInstanceOf[Term]
  override def cases: List[CaseDef] = underlying.cases.map(CaseDefImpl(_))
  override def isInline: Boolean = underlying.isInline
}
object MatchImpl {
  object Module extends pub.Match.Module {
    override def apply(selector: pub.Term, cases: List[pub.CaseDef]): pub.Match = ???
    override def make(selector: pub.Term, cases: List[pub.CaseDef]): pub.Match = ???
  }
}

/////// SummonFrom ///////////////////////////////////////////////////////////////

type SummonFrom = SummonFromImpl
final class SummonFromImpl(val underlying: tpd.Match) extends TermImpl, pub.SummonFrom {
  override def cases: List[CaseDef] = underlying.cases.map(CaseDefImpl(_))
}
object SummonFromImpl {
  object Module extends pub.SummonFrom.Module {
    override def apply(cases: List[pub.CaseDef]): pub.SummonFrom = ???
    override def make(cases: List[pub.CaseDef]): pub.SummonFrom = ???
  }
}

/////// Try ///////////////////////////////////////////////////////////////

type Try = TryImpl
final class TryImpl(val underlying: tpd.Try) extends TermImpl, pub.Try {
  override def body: Term = TreeImpl(underlying.expr).asInstanceOf[Term]
  override def cases: List[CaseDef] = underlying.cases.map(CaseDefImpl(_))
  override def finalizer: Option[Term] = TreeImpl.optional(underlying.finalizer).map(TreeImpl(_).asInstanceOf[Term])
}
object TryImpl {
  object Module extends pub.Try.Module {
    override def apply(expr: pub.Term, cases: List[pub.CaseDef], finalizer: Option[pub.Term]): pub.Try = ???
    override def make(expr: pub.Term, cases: List[pub.CaseDef], finalizer: Option[pub.Term]): pub.Try = ???
  }
}

/////// Return ///////////////////////////////////////////////////////////////

type Return = ReturnImpl
final class ReturnImpl(val underlying: tpd.Return) extends TermImpl, pub.Return {
  override def expr: Term = TreeImpl(underlying.expr).asInstanceOf[Term]
  override def from: Symbol = SymbolImpl(underlying.from.symbol)
}
object ReturnImpl {
  object Module extends pub.Return.Module {
    override def apply(expr: pub.Term, from: pub.Symbol): pub.Return = ???
    override def make(expr: pub.Term, from: pub.Symbol): pub.Return = ???
  }
}

/////// Repeated ///////////////////////////////////////////////////////////////

type Repeated = RepeatedImpl
final class RepeatedImpl(val underlying: tpd.SeqLiteral) extends TermImpl, pub.Repeated {
  override def elems: List[Term] = underlying.elems.map(TreeImpl(_).asInstanceOf[Term])
  override def elemtpt: TypeTree = TreeImpl(underlying.elemtpt).asInstanceOf[TypeTree]
}
object RepeatedImpl {
  object Module extends pub.Repeated.Module {
    override def apply(elems: List[pub.Term], tpt: pub.TypeTree): pub.Repeated = ???
    override def make(elems: List[pub.Term], tpt: pub.TypeTree): pub.Repeated = ???
  }
}

/////// Inlined ///////////////////////////////////////////////////////////////

type Inlined = InlinedImpl
final class InlinedImpl(val underlying: tpd.Inlined) extends TermImpl, pub.Inlined {
  override def call: Option[Tree] = TreeImpl.optional(underlying.call).map(TreeImpl(_))
  override def bindings: List[Definition] = underlying.bindings.map(TreeImpl(_).asInstanceOf[Definition])
  override def body: Term = TreeImpl(underlying.expansion).asInstanceOf[Term]
}
object InlinedImpl {
  object Module extends pub.Inlined.Module {
    override def apply(call: Option[pub.Tree], bindings: List[pub.Definition], expansion: pub.Term): pub.Inlined = ???
    override def make(call: Option[pub.Tree], bindings: List[pub.Definition], expansion: pub.Term): pub.Inlined = ???
  }
}

/////// SelectOuter ///////////////////////////////////////////////////////////////

type SelectOuter = SelectOuterImpl
final class SelectOuterImpl(val underlying: tpd.Select) extends TermImpl, pub.SelectOuter {
  override def qualifier: Term = TreeImpl(underlying.qualifier).asInstanceOf[Term]
  override def name: String = underlying.name.toString
  override def level: Int =
    val NameKinds.OuterSelectName(_, levels) = underlying.name: @unchecked
    levels
}
object SelectOuterImpl {
  object Module extends pub.SelectOuter.Module {
    override def apply(qualifier: pub.Term, name: String, levels: Int): pub.SelectOuter = ???
    override def make(qualifier: pub.Term, name: String, levels: Int): pub.SelectOuter = ???
  }
}

/////// While ///////////////////////////////////////////////////////////////

type While = WhileImpl
final class WhileImpl(val underlying: tpd.WhileDo) extends TermImpl, pub.While {
  override def cond: Term = TreeImpl(underlying.cond).asInstanceOf[Term]
  override def body: Term = TreeImpl(underlying.body).asInstanceOf[Term]
}
object WhileImpl {
  object Module extends pub.While.Module {
    override def apply(cond: pub.Term, body: pub.Term): pub.While = ???
    override def make(cond: pub.Term, body: pub.Term): pub.While = ???
  }
}

/////// TypedOrTest ///////////////////////////////////////////////////////////////

type TypedOrTest = pub.TypedOrTest & TypedOrTestImpl
sealed trait TypedOrTestImpl extends TreeImpl { _self: pub.TypedOrTest =>
  override val underlying: tpd.Typed
  override def tree: Tree = TreeImpl(underlying.expr)
  override def tpt: TypeTree = TreeImpl(underlying.tpt).asInstanceOf[TypeTree]
}
object TypedOrTestImpl {
  object Module extends pub.TypedOrTest.Module {
    override def apply(expr: pub.Tree, tpt: pub.TypeTree): pub.TypedOrTest = ???
    override def make(expr: pub.Tree, tpt: pub.TypeTree): pub.TypedOrTest = ???
  }
}

/////// TypeTree ///////////////////////////////////////////////////////////////

type TypeTree = pub.TypeTree & TypeTreeImpl
sealed trait TypeTreeImpl extends TreeImpl { _self: pub.TypeTree =>
  override def tpe: TypeRepr = TypeReprImpl(underlying.tpe.stripTypeVar)
}
object TypeTreeImpl {
  object Module extends pub.TypeTree.Module {
    override def of[T <: AnyKind](using Type[T]): pub.TypeTree = ???
    override def ref(typeSymbol: pub.Symbol): pub.TypeTree = ???
  }
}

/////// Inferred ///////////////////////////////////////////////////////////////

type Inferred = InferredImpl
final class InferredImpl(val underlying: tpd.TypeTree) extends TypeTreeImpl, pub.Inferred
object InferredImpl {
  object Module extends pub.Inferred.Module {
    override def apply(tpe: pub.TypeRepr): pub.Inferred = ???
    override def make(tpe: pub.TypeRepr): pub.Inferred = ???
  }
}

/////// TypeIdent ///////////////////////////////////////////////////////////////

type TypeIdent = TypeIdentImpl
final class TypeIdentImpl(val underlying: tpd.Ident) extends TypeTreeImpl, pub.TypeIdent {
  override def name: String = underlying.name.toString
}
object TypeIdentImpl {
  object Module extends pub.TypeIdent.Module {
    override def apply(sym: pub.Symbol): pub.TypeTree = ???
    override def make(sym: pub.Symbol): pub.TypeTree = ???
  }
}

/////// TypeSelect ///////////////////////////////////////////////////////////////

type TypeSelect = TypeSelectImpl
final class TypeSelectImpl(val underlying: tpd.Select) extends TypeTreeImpl, pub.TypeSelect {
  override def qualifier: Term = TreeImpl(underlying.qualifier).asInstanceOf[Term]
  override def name: String = underlying.name.toString
}
object TypeSelectImpl {
  object Module extends pub.TypeSelect.Module {
    override def apply(qualifier: pub.Term, name: String): pub.TypeSelect = ???
    override def make(qualifier: pub.Term, name: String): pub.TypeSelect = ???
  }
}

/////// TypeProjection ///////////////////////////////////////////////////////////////

type TypeProjection = TypeProjectionImpl
final class TypeProjectionImpl(val underlying: tpd.Select) extends TypeTreeImpl, pub.TypeProjection {
  override def qualifier: TypeTree = TreeImpl(underlying.qualifier).asInstanceOf[TypeTree]
  override def name: String = underlying.name.toString
}
object TypeProjectionImpl {
  object Module extends pub.TypeProjection.Module {
    override def apply(qualifier: pub.TypeTree, name: String): pub.TypeProjection = ???
    override def make(qualifier: pub.TypeTree, name: String): pub.TypeProjection = ???
  }
}

/////// Singleton ///////////////////////////////////////////////////////////////

type Singleton = SingletonImpl
final class SingletonImpl(val underlying: tpd.SingletonTypeTree) extends TypeTreeImpl, pub.Singleton {
  override def ref: Term = TreeImpl(underlying.ref).asInstanceOf[Term]
}
object SingletonImpl {
  object Module extends pub.Singleton.Module {
    override def apply(ref: pub.Term): pub.Singleton = ???
    override def make(ref: pub.Term): pub.Singleton = ???
  }
}

/////// Refined ///////////////////////////////////////////////////////////////

type Refined = RefinedImpl
final class RefinedImpl(val underlying: tpd.RefinedTypeTree) extends TypeTreeImpl, pub.Refined {
  override def tpt: TypeTree = TreeImpl(underlying.tpt).asInstanceOf[TypeTree]
  override def refinements: List[Definition] = underlying.refinements.map(TreeImpl(_).asInstanceOf[Definition])
}
object RefinedImpl {
  object Module extends pub.Refined.Module {
    override def apply(tpt: pub.TypeTree, refinements: List[pub.Definition], refineCls: pub.Symbol): pub.Refined = ???
    override def make(tpt: pub.TypeTree, refinements: List[pub.Definition], refineCls: pub.Symbol): pub.Refined = ???
  }
}

/////// Applied ///////////////////////////////////////////////////////////////

type Applied = AppliedImpl
final class AppliedImpl(val underlying: tpd.AppliedTypeTree) extends TypeTreeImpl, pub.Applied {
  override def tpt: TypeTree = TreeImpl(underlying.tpt).asInstanceOf[TypeTree]
  override def args: List[Tree] = underlying.args.map(TreeImpl(_))
}
object AppliedImpl {
  object Module extends pub.Applied.Module {
    override def apply(tpt: pub.TypeTree, args: List[pub.Tree]): pub.Applied = ???
    override def make(tpt: pub.TypeTree, args: List[pub.Tree]): pub.Applied = ???
  }
}

/////// Annotated ///////////////////////////////////////////////////////////////

type Annotated = AnnotatedImpl
final class AnnotatedImpl(val underlying: tpd.Annotated) extends TypeTreeImpl, pub.Annotated {
  override def arg: TypeTree = TreeImpl(underlying.arg).asInstanceOf[TypeTree]
  override def annotation: Term = TreeImpl(underlying.annot).asInstanceOf[Term]
}
object AnnotatedImpl {
  object Module extends pub.Annotated.Module {
    override def apply(arg: pub.TypeTree, annotation: pub.Term): pub.Annotated = ???
    override def make(arg: pub.TypeTree, annotation: pub.Term): pub.Annotated = ???
  }
}

/////// MatchTypeTree ///////////////////////////////////////////////////////////////

type MatchTypeTree = MatchTypeTreeImpl
final class MatchTypeTreeImpl(val underlying: tpd.MatchTypeTree) extends TypeTreeImpl, pub.MatchTypeTree {
  override def bound: Option[TypeTree] = TreeImpl.optional(underlying.bound).map(TreeImpl(_).asInstanceOf[TypeTree])
  override def selector: TypeTree = TreeImpl(underlying.selector).asInstanceOf[TypeTree]
  override def cases: List[TypeCaseDef] = underlying.cases.map(TypeCaseDefImpl(_))
}
object MatchTypeTreeImpl {
  object Module extends pub.MatchTypeTree.Module {
    override def apply(bound: Option[pub.TypeTree], selector: pub.TypeTree, cases: List[pub.TypeCaseDef]): pub.MatchTypeTree = ???
    override def make(bound: Option[pub.TypeTree], selector: pub.TypeTree, cases: List[pub.TypeCaseDef]): pub.MatchTypeTree = ???
  }
}

/////// ByName ///////////////////////////////////////////////////////////////

type ByName = ByNameImpl
final class ByNameImpl(val underlying: tpd.ByNameTypeTree) extends TypeTreeImpl, pub.ByName {
  override def result: TypeTree = TreeImpl(underlying.result).asInstanceOf[TypeTree]
}
object ByNameImpl {
  object Module extends pub.ByName.Module {
    override def apply(result: pub.TypeTree): pub.ByName = ???
    override def make(result: pub.TypeTree): pub.ByName = ???
  }
}

/////// LambdaTypeTree ///////////////////////////////////////////////////////////////

type LambdaTypeTree = LambdaTypeTreeImpl
final class LambdaTypeTreeImpl(val underlying: tpd.LambdaTypeTree) extends TypeTreeImpl, pub.LambdaTypeTree {
  override def tparams: List[TypeDef] = underlying.tparams.map(TypeDefImpl(_))
  override def body: Tree = TreeImpl(underlying.body)
}
object LambdaTypeTreeImpl {
  object Module extends pub.LambdaTypeTree.Module {
    override def apply(tparams: List[pub.TypeDef], body: pub.Tree): pub.LambdaTypeTree = ???
    override def make(tparams: List[pub.TypeDef], body: pub.Tree): pub.LambdaTypeTree = ???
  }
}

/////// TypeBind ///////////////////////////////////////////////////////////////

type TypeBind = TypeBindImpl
final class TypeBindImpl(val underlying: tpd.Bind) extends TypeTreeImpl, pub.TypeBind {
  override def name: String = underlying.name.toString
  override def body: Tree = TreeImpl(underlying.body)
}
object TypeBindImpl {
  object Module extends pub.TypeBind.Module {}
}

/////// TypeBlock ///////////////////////////////////////////////////////////////

type TypeBlock = TypeBlockImpl
final class TypeBlockImpl(val underlying: tpd.Block) extends TypeTreeImpl, pub.TypeBlock {
  override def aliases: List[TypeDef] = underlying.stats.map(TreeImpl(_).asInstanceOf[TypeDef])
  override def tpt: TypeTree = TreeImpl(underlying.expr).asInstanceOf[TypeTree]
}
object TypeBlockImpl {
  object Module extends pub.TypeBlock.Module {
    override def apply(aliases: List[pub.TypeDef], tpt: pub.TypeTree): pub.TypeBlock = ???
    override def make(aliases: List[pub.TypeDef], tpt: pub.TypeTree): pub.TypeBlock = ???
  }
}

/////// TypeBoundsTree ///////////////////////////////////////////////////////////////

type TypeBoundsTree = TypeBoundsTreeImpl
final class TypeBoundsTreeImpl(val underlying: tpd.Tree) extends TreeImpl, pub.TypeBoundsTree {
  override def tpe: TypeBounds = TypeBoundsImpl(underlying.tpe.asInstanceOf[dotc.core.Types.TypeBounds])
  override def low: TypeTree = underlying match
    case self: tpd.TypeBoundsTree => TreeImpl(self.lo).asInstanceOf[TypeTree]
    case self: tpd.TypeTree       => ??? // Cannot implement without Context to makeTypeDef
  override def hi: TypeTree = underlying match
    case self: tpd.TypeBoundsTree => TreeImpl(self.hi).asInstanceOf[TypeTree]
    case self: tpd.TypeTree       => ??? // Cannot implement without Context to makeTypeDef
}
object TypeBoundsTreeImpl {
  object Module extends pub.TypeBoundsTree.Module {
    override def apply(low: pub.TypeTree, hi: pub.TypeTree): pub.TypeBoundsTree = ???
    override def make(low: pub.TypeTree, hi: pub.TypeTree): pub.TypeBoundsTree = ???
  }
}

/////// WildcardTypeTree ///////////////////////////////////////////////////////////////

type WildcardTypeTree = WildcardTypeTreeImpl
final class WildcardTypeTreeImpl(val underlying: tpd.Ident) extends TreeImpl, pub.WildcardTypeTree {
  override def tpe: TypeRepr = TypeReprImpl(underlying.tpe.stripTypeVar)
}
object WildcardTypeTreeImpl {
  object Module extends pub.WildcardTypeTree.Module {
    override def apply(tpe: pub.TypeRepr): pub.WildcardTypeTree = ???
    override def make(tpe: pub.TypeRepr): pub.WildcardTypeTree = ???
  }
}

/////// CaseDef ///////////////////////////////////////////////////////////////

type CaseDef = CaseDefImpl
final class CaseDefImpl(val underlying: tpd.CaseDef) extends TreeImpl, pub.CaseDef {
  override def pattern: Tree = TreeImpl(underlying.pat)
  override def guard: Option[Term] = TreeImpl.optional(underlying.guard).map(TreeImpl(_).asInstanceOf[Term])
  override def rhs: Term = TreeImpl(underlying.body).asInstanceOf[Term]
}
object CaseDefImpl {
  object Module extends pub.CaseDef.Module {
    override def apply(pattern: pub.Tree, guard: Option[pub.Term], rhs: pub.Term): pub.CaseDef = ???
    override def make(pattern: pub.Tree, guard: Option[pub.Term], rhs: pub.Term): pub.CaseDef = ???
  }
}

/////// TypeCaseDef ///////////////////////////////////////////////////////////////

type TypeCaseDef = TypeCaseDefImpl
final class TypeCaseDefImpl(val underlying: tpd.CaseDef) extends TreeImpl, pub.TypeCaseDef {
  override def pattern: TypeTree = TreeImpl(underlying.pat).asInstanceOf[TypeTree]
  override def rhs: TypeTree = TreeImpl(underlying.body).asInstanceOf[TypeTree]
}
object TypeCaseDefImpl {
  object Module extends pub.TypeCaseDef.Module {
    override def apply(pattern: pub.TypeTree, rhs: pub.TypeTree): pub.TypeCaseDef = ???
    override def make(pattern: pub.TypeTree, rhs: pub.TypeTree): pub.TypeCaseDef = ???
  }
}

/////// Bind ///////////////////////////////////////////////////////////////

type Bind = BindImpl
final class BindImpl(val underlying: tpd.Bind) extends TreeImpl, pub.Bind {
  override def name: String = underlying.name.toString
  override def pattern: Tree = TreeImpl(underlying.body)
}
object BindImpl {
  object Module extends pub.Bind.Module {
    override def apply(sym: pub.Symbol, pattern: pub.Tree): pub.Bind = ???
    override def make(sym: pub.Symbol, pattern: pub.Tree): pub.Bind = ???
  }
}

/////// Unapply ///////////////////////////////////////////////////////////////

type Unapply = UnapplyImpl
final class UnapplyImpl(val underlying: tpd.Tree) extends TreeImpl, pub.Unapply {
  override def fun: Term =
    val t = underlying match
      case self: tpd.UnApply      => self.fun
      case self: tpd.QuotePattern => dotc.quoted.QuotePatterns.encode(self).fun
    TreeImpl(t).asInstanceOf[Term]

  override def implicits: List[Term] =
    val ts = underlying match
      case self: tpd.UnApply      => self.implicits
      case self: tpd.QuotePattern => dotc.quoted.QuotePatterns.encode(self).implicits
    ts.map(TreeImpl(_).asInstanceOf[Term])

  override def patterns: List[Tree] =
    val ps = underlying match
      case self: tpd.UnApply      => self.patterns
      case self: tpd.QuotePattern => dotc.quoted.QuotePatterns.encode(self).patterns
    val effective = ps match
      case patterns0 :+ dotc.ast.Trees.SeqLiteral(elems, _) => patterns0 ::: elems
      case _                                                => ps
    effective.map(TreeImpl(_))
}
object UnapplyImpl {
  object Module extends pub.Unapply.Module {
    override def apply(fun: pub.Term, implicits: List[pub.Term], patterns: List[pub.Tree]): pub.Unapply = ???
    override def make(fun: pub.Term, implicits: List[pub.Term], patterns: List[pub.Tree]): pub.Unapply = ???
  }
}

/////// Alternatives ///////////////////////////////////////////////////////////////

type Alternatives = AlternativesImpl
final class AlternativesImpl(val underlying: tpd.Alternative) extends TreeImpl, pub.Alternatives {
  override def patterns: List[Tree] = underlying.trees.map(TreeImpl(_))
}
object AlternativesImpl {
  object Module extends pub.Alternatives.Module {
    override def apply(patterns: List[pub.Tree]): pub.Alternatives = ???
    override def make(patterns: List[pub.Tree]): pub.Alternatives = ???
  }
}
