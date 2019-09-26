package dotty.tools.dotc
package tastyreflect

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{TreeTypeMap, Trees, tpd, untpd}
import dotty.tools.dotc.typer.{Implicits, Typer}
import dotty.tools.dotc.core._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.quoted.PickledQuotes
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Types.SingletonType
import dotty.tools.dotc.tastyreflect.FromSymbol.{definitionFromSym, packageDefFromSym}
import dotty.tools.dotc.typer.Implicits.{AmbiguousImplicits, DivergingImplicit, NoMatchingImplicits, SearchFailure, SearchFailureType}
import dotty.tools.dotc.util.{SourceFile, SourcePosition, Spans}

import scala.runtime.quoted.Unpickler
import scala.tasty.reflect.CompilerInterface

class ReflectionCompilerInterface(val rootContext: core.Contexts.Context) extends CompilerInterface {
  import tpd._

  private implicit def ctx: core.Contexts.Context = rootContext

  def settings: Settings = rootContext.settings

  def rootPosition: util.SourcePosition =
    tastyreflect.MacroExpansion.position.getOrElse(SourcePosition(rootContext.source, Spans.NoSpan))

  //
  // QUOTE UNPICKLING
  //

  def unpickleExpr(repr: Unpickler.PickledQuote, args: Unpickler.PickledExprArgs): scala.quoted.Expr[?] =
    new scala.internal.quoted.TastyTreeExpr(PickledQuotes.unpickleExpr(repr, args), compilerId)

  def unpickleType(repr: Unpickler.PickledQuote, args: Unpickler.PickledTypeArgs): scala.quoted.Type[?] =
    new scala.internal.quoted.TreeType(PickledQuotes.unpickleType(repr, args), compilerId)

  //
  // CONTEXT
  //

  type Context = core.Contexts.Context

  def Context_owner(self: Context): Symbol = self.owner

  def Context_source(self: Context): java.nio.file.Path = self.compilationUnit.source.file.jpath

  def Context_GADT_setFreshGADTBounds(self: Context): Context =
    self.fresh.setFreshGADTBounds.addMode(Mode.GadtConstraintInference)

  def Context_GADT_addToConstraint(self: Context)(syms: List[Symbol]): Boolean =
    self.gadt.addToConstraint(syms)

  def Context_GADT_approximation(self: Context)(sym: Symbol, fromBelow: Boolean): Type =
    self.gadt.approximation(sym, fromBelow)

  //
  // REPORTING
  //

  def error(msg: => String, pos: Position)(given ctx: Context): Unit =
    ctx.error(msg, pos)

  def error(msg: => String, sourceFile: SourceFile, start: Int, end: Int)(given ctx: Context): Unit =
    ctx.error(msg, util.SourcePosition(sourceFile, util.Spans.Span(start, end)))

  def warning(msg: => String, pos: Position)(given ctx: Context): Unit =
    ctx.warning(msg, pos)

  def warning(msg: => String, sourceFile: SourceFile, start: Int, end: Int)(given ctx: Context): Unit =
    ctx.error(msg, util.SourcePosition(sourceFile, util.Spans.Span(start, end)))

  //
  // Settings
  //

  type Settings = config.ScalaSettings

  def Settings_color(self: Settings): Boolean = self.color.value(rootContext) == "always"

  //
  // TREES
  //

  type Tree = tpd.Tree

  def Tree_pos(self: Tree)(given Context): Position = self.sourcePos
  def Tree_symbol(self: Tree)(given Context): Symbol = self.symbol

  type PackageClause = tpd.PackageDef

  def matchPackageClause(tree: Tree)(given Context): Option[PackageClause] = tree match {
    case x: tpd.PackageDef => Some(x)
    case _ => None
  }

  def PackageClause_pid(self: PackageClause)(given Context): Ref = self.pid
  def PackageClause_stats(self: PackageClause)(given Context): List[Tree] = self.stats

  def PackageClause_apply(pid: Ref, stats: List[Tree])(given Context): PackageClause =
    withDefaultPos(tpd.PackageDef(pid.asInstanceOf[tpd.RefTree], stats))

  def PackageClause_copy(original: PackageClause)(pid: Ref, stats: List[Tree])(given Context): PackageClause =
    tpd.cpy.PackageDef(original)(pid, stats)

  type Statement = tpd.Tree

  def matchStatement(tree: Tree)(given Context): Option[Statement] = tree match {
    case tree if tree.isTerm => Some(tree)
    case _ => matchDefinition(tree)
  }

  type Import = tpd.Import

  def matchImport(tree: Tree)(given Context): Option[Import] = tree match {
    case tree: tpd.Import => Some(tree)
    case _ => None
  }

  def Import_implied(self: Import): Boolean = false // TODO: adapt to new import scheme
  def Import_expr(self: Import)(given Context): Tree = self.expr
  def Import_selectors(self: Import)(given Context): List[ImportSelector] = self.selectors

  def Import_apply(expr: Term, selectors: List[ImportSelector])(given Context): Import =
    withDefaultPos(tpd.Import(expr, selectors))

  def Import_copy(original: Import)(expr: Term, selectors: List[ImportSelector])(given Context): Import =
    tpd.cpy.Import(original)(expr, selectors)

  type Definition = tpd.Tree

  def matchDefinition(tree: Tree)(given Context): Option[Definition] = tree match {
    case tree: tpd.MemberDef => Some(tree)
    case tree: PackageDefinition => Some(tree)
    case _ => None
  }

  def Definition_name(self: Definition)(given Context): String = self match {
    case self: tpd.MemberDef => self.name.toString
    case self: PackageDefinition => self.symbol.name.toString // TODO make PackageDefinition a MemberDef or NameTree
  }

  type PackageDef = PackageDefinition

  def matchPackageDef(tree: Tree)(given Context): Option[PackageDef] = tree match {
    case x: PackageDefinition => Some(x)
    case _ => None
  }

  def PackageDef_owner(self: PackageDef)(given Context): PackageDef = packageDefFromSym(self.symbol.owner)

  def PackageDef_members(self: PackageDef)(given Context): List[Statement] =
    if (self.symbol.is(core.Flags.JavaDefined)) Nil // FIXME should also support java packages
    else self.symbol.info.decls.iterator.map(definitionFromSym).toList

  def PackageDef_symbol(self: PackageDef)(given Context): Symbol = self.symbol

  type ClassDef = tpd.TypeDef

  def matchClassDef(tree: Tree)(given Context): Option[ClassDef] = tree match {
    case x: tpd.TypeDef if x.isClassDef => Some(x)
    case _ => None
  }

  def ClassDef_constructor(self: ClassDef)(given Context): DefDef = ClassDef_rhs(self).constr
  def ClassDef_parents(self: ClassDef)(given Context): List[Term | TypeTree] = ClassDef_rhs(self).parents
  def ClassDef_derived(self: ClassDef)(given Context): List[TypeTree] = ClassDef_rhs(self).derived.asInstanceOf[List[TypeTree]]
  def ClassDef_self(self: ClassDef)(given Context): Option[ValDef] = optional(ClassDef_rhs(self).self)
  def ClassDef_body(self: ClassDef)(given Context): List[Statement] = ClassDef_rhs(self).body
  def ClassDef_symbol(self: ClassDef)(given Context): Symbol = self.symbol.asClass
  private def ClassDef_rhs(self: ClassDef) = self.rhs.asInstanceOf[tpd.Template]

  def ClassDef_copy(original: ClassDef)(name: String, constr: DefDef, parents: List[Term | TypeTree], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement])(given Context): ClassDef = {
    val Trees.TypeDef(_, originalImpl: tpd.Template) = original
    tpd.cpy.TypeDef(original)(name.toTypeName, tpd.cpy.Template(originalImpl)(constr, parents, derived, selfOpt.getOrElse(tpd.EmptyValDef), body))
  }

  type TypeDef = tpd.TypeDef

  def matchTypeDef(tree: Tree)(given Context): Option[TypeDef] = tree match {
    case x: tpd.TypeDef if !x.symbol.isClass => Some(x)
    case _ => None
  }

  def TypeDef_rhs(self: TypeDef)(given Context): TypeTree | TypeBoundsTree = self.rhs
  def TypeDef_symbol(self: TypeDef)(given Context): Symbol = self.symbol.asType

  def TypeDef_apply(symbol: Symbol)(given Context): TypeDef = withDefaultPos(tpd.TypeDef(symbol.asType))
  def TypeDef_copy(original: TypeDef)(name: String, rhs: TypeTree | TypeBoundsTree)(given Context): TypeDef =
    tpd.cpy.TypeDef(original)(name.toTypeName, rhs)

  type DefDef = tpd.DefDef

  def matchDefDef(tree: Tree)(given Context): Option[DefDef] = tree match {
    case x: tpd.DefDef => Some(x)
    case _ => None
  }

  def DefDef_typeParams(self: DefDef)(given Context): List[TypeDef] = self.tparams
  def DefDef_paramss(self: DefDef)(given Context): List[List[ValDef]] = self.vparamss
  def DefDef_returnTpt(self: DefDef)(given Context): TypeTree = self.tpt
  def DefDef_rhs(self: DefDef)(given Context): Option[Tree] = optional(self.rhs)
  def DefDef_symbol(self: DefDef)(given Context): Symbol = self.symbol.asTerm

  def DefDef_apply(symbol: Symbol, rhsFn: List[Type] => List[List[Term]] => Option[Term])(given Context): DefDef =
    withDefaultPos(tpd.polyDefDef(symbol.asTerm, tparams => vparamss => rhsFn(tparams)(vparamss).getOrElse(tpd.EmptyTree)))

  def DefDef_copy(original: DefDef)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term])(given Context): DefDef =
    tpd.cpy.DefDef(original)(name.toTermName, typeParams, paramss, tpt, rhs.getOrElse(tpd.EmptyTree))

  type ValDef = tpd.ValDef

  def matchValDef(tree: Tree)(given Context): Option[ValDef] = tree match {
    case x: tpd.ValDef => Some(x)
    case _ => None
  }

  def ValDef_tpt(self: ValDef)(given Context): TypeTree = self.tpt
  def ValDef_rhs(self: ValDef)(given Context): Option[Tree] = optional(self.rhs)
  def ValDef_symbol(self: ValDef)(given Context): Symbol = self.symbol.asTerm

  def ValDef_apply(symbol: Symbol, rhs: Option[Term])(given Context): ValDef =
    tpd.ValDef(symbol.asTerm, rhs.getOrElse(tpd.EmptyTree))

  def ValDef_copy(original: ValDef)(name: String, tpt: TypeTree, rhs: Option[Term])(given Context): ValDef =
    tpd.cpy.ValDef(original)(name.toTermName, tpt, rhs.getOrElse(tpd.EmptyTree))

  type Term = tpd.Tree

  def matchTerm(tree: Tree)(given Context): Option[Term] = tree match {
    case x: tpd.SeqLiteral => Some(tree)
    case _ if tree.isTerm => Some(tree)
    case _ => None
  }

  def Term_pos(self: Term)(given Context): Position = self.sourcePos
  def Term_tpe(self: Term)(given Context): Type = self.tpe
  def Term_underlyingArgument(self: Term)(given Context): Term = self.underlyingArgument
  def Term_underlying(self: Term)(given Context): Term = self.underlying

  type Ref = tpd.RefTree

  def matchRef(tree: Tree)(given Context): Option[Ref] = tree match {
    case x: tpd.RefTree if x.isTerm => Some(x)
    case _ => None
  }

  def Ref_apply(sym: Symbol)(given Context): Ref =
    withDefaultPos(tpd.ref(sym).asInstanceOf[tpd.RefTree])

  type Ident = tpd.Ident

  def matchIdent(x: Term)(given Context): Option[Ident] = x match {
    case x: tpd.Ident if x.isTerm => Some(x)
    case _ => None
  }

  def Ident_name(self: Ident)(given Context): String = self.name.show

  def Ident_apply(tmref: TermRef)(given Context): Term =
    withDefaultPos(tpd.ref(tmref).asInstanceOf[Term])

  def Ident_copy(original: Tree)(name: String)(given Context): Ident =
    tpd.cpy.Ident(original)(name.toTermName)

  type Select = tpd.Select

  def matchSelect(x: Term)(given Context): Option[Select] = x match {
    case x: tpd.Select if x.isTerm => Some(x)
    case _ => None
  }

  def Select_qualifier(self: Select)(given Context): Term = self.qualifier
  def Select_name(self: Select)(given Context): String = self.name.toString
  def Select_signature(self: Select)(given Context): Option[Signature] =
    if (self.symbol.signature == core.Signature.NotAMethod) None
    else Some(self.symbol.signature)

  def Select_apply(qualifier: Term, symbol: Symbol)(given Context): Select =
    withDefaultPos(tpd.Select(qualifier, Types.TermRef(qualifier.tpe, symbol)))

  def Select_unique(qualifier: Term, name: String)(given Context): Select = {
    val denot = qualifier.tpe.member(name.toTermName)
    assert(!denot.isOverloaded, s"The symbol `$name` is overloaded. The method Select.unique can only be used for non-overloaded symbols.")
    withDefaultPos(tpd.Select(qualifier, name.toTermName))
  }

  // TODO rename, this returns an Apply and not a Select
  def Select_overloaded(qualifier: Term, name: String, targs: List[Type], args: List[Term])(given Context): Apply =
    withDefaultPos(tpd.applyOverloaded(qualifier, name.toTermName, args, targs, Types.WildcardType).asInstanceOf[Apply])

  def Select_copy(original: Tree)(qualifier: Term, name: String)(given Context): Select =
    tpd.cpy.Select(original)(qualifier, name.toTermName)

  type Literal = tpd.Literal

  def matchLiteral(x: Term)(given Context): Option[Literal] = x match {
    case x: tpd.Literal => Some(x)
    case _ => None
  }

  def Literal_constant(self: Literal)(given Context): Constant = self.const

  def Literal_apply(constant: Constant)(given Context): Literal =
    withDefaultPos(tpd.Literal(constant))

  def Literal_copy(original: Tree)(constant: Constant)(given Context): Literal =
    tpd.cpy.Literal(original)(constant)

  type This = tpd.This

  def matchThis(x: Term)(given Context): Option[This] = x match {
    case x: tpd.This => Some(x)
    case _ => None
  }

  def This_id(self: This)(given Context): Option[Id] = optional(self.qual)

  def This_apply(cls: Symbol)(given Context): This =
    withDefaultPos(tpd.This(cls.asClass))

  def This_copy(original: Tree)(qual: Option[Id])(given Context): This =
    tpd.cpy.This(original)(qual.getOrElse(untpd.EmptyTypeIdent))

  type New = tpd.New

  def matchNew(x: Term)(given Context): Option[New] = x match {
    case x: tpd.New => Some(x)
    case _ => None
  }

  def New_tpt(self: New)(given Context): TypeTree = self.tpt

  def New_apply(tpt: TypeTree)(given Context): New = withDefaultPos(tpd.New(tpt))

  def New_copy(original: Tree)(tpt: TypeTree)(given Context): New =
    tpd.cpy.New(original)(tpt)

  type NamedArg = tpd.NamedArg

  def matchNamedArg(x: Term)(given Context): Option[NamedArg] = x match {
    case x: tpd.NamedArg if x.name.isInstanceOf[core.Names.TermName] => Some(x) // TODO: Now, the name should alwas be a term name
    case _ => None
  }

  def NamedArg_name(self: NamedArg)(given Context): String = self.name.toString
  def NamedArg_value(self: NamedArg)(given Context): Term = self.arg

  def NamedArg_apply(name: String, arg: Term)(given Context): NamedArg =
    withDefaultPos(tpd.NamedArg(name.toTermName, arg))

  def NamedArg_copy(tree: NamedArg)(name: String, arg: Term)(given Context): NamedArg =
    tpd.cpy.NamedArg(tree)(name.toTermName, arg)

  type Apply = tpd.Apply

  def matchApply(x: Term)(given Context): Option[Apply] = x match {
    case x: tpd.Apply => Some(x)
    case _ => None
  }

  def Apply_fun(self: Apply)(given Context): Term = self.fun
  def Apply_args(self: Apply)(given Context): List[Term] = self.args


  def Apply_apply(fn: Term, args: List[Term])(given Context): Apply =
    withDefaultPos(tpd.Apply(fn, args))

  def Apply_copy(original: Tree)(fun: Term, args: List[Term])(given Context): Apply =
    tpd.cpy.Apply(original)(fun, args)

  type TypeApply = tpd.TypeApply

  def matchTypeApply(x: Term)(given Context): Option[TypeApply] = x match {
    case x: tpd.TypeApply => Some(x)
    case _ => None
  }

  def TypeApply_fun(self: TypeApply)(given Context): Term = self.fun
  def TypeApply_args(self: TypeApply)(given Context): List[TypeTree] = self.args

  def TypeApply_apply(fn: Term, args: List[TypeTree])(given Context): TypeApply =
    withDefaultPos(tpd.TypeApply(fn, args))

  def TypeApply_copy(original: Tree)(fun: Term, args: List[TypeTree])(given Context): TypeApply =
    tpd.cpy.TypeApply(original)(fun, args)

  type Super = tpd.Super

  def matchSuper(x: Term)(given Context): Option[Super] = x match {
    case x: tpd.Super => Some(x)
    case _ => None
  }

  def Super_qualifier(self: Super)(given Context): Term = self.qual
  def Super_id(self: Super)(given Context): Option[Id] = optional(self.mix)

  def Super_apply(qual: Term, mix: Option[Id])(given Context): Super =
    withDefaultPos(tpd.Super(qual, mix.getOrElse(untpd.EmptyTypeIdent), false, NoSymbol))

  def Super_copy(original: Tree)(qual: Term, mix: Option[Id])(given Context): Super =
    tpd.cpy.Super(original)(qual, mix.getOrElse(untpd.EmptyTypeIdent))

  type Typed = tpd.Typed

  def matchTyped(x: Term)(given Context): Option[Typed] = x match {
    case x: tpd.Typed => Some(x)
    case _ => None
  }

  def Typed_expr(self: Typed)(given Context): Term = self.expr
  def Typed_tpt(self: Typed)(given Context): TypeTree = self.tpt

  def Typed_apply(expr: Term, tpt: TypeTree)(given Context): Typed =
    withDefaultPos(tpd.Typed(expr, tpt))

  def Typed_copy(original: Tree)(expr: Term, tpt: TypeTree)(given Context): Typed =
    tpd.cpy.Typed(original)(expr, tpt)

  type Assign = tpd.Assign

  def matchAssign(x: Term)(given Context): Option[Assign] = x match {
    case x: tpd.Assign => Some(x)
    case _ => None
  }

  def Assign_lhs(self: Assign)(given Context): Term = self.lhs
  def Assign_rhs(self: Assign)(given Context): Term = self.rhs

  def Assign_apply(lhs: Term, rhs: Term)(given Context): Assign =
    withDefaultPos(tpd.Assign(lhs, rhs))

  def Assign_copy(original: Tree)(lhs: Term, rhs: Term)(given Context): Assign =
    tpd.cpy.Assign(original)(lhs, rhs)

  type Block = tpd.Block

  def matchBlock(x: Term)(given Context): Option[Block] = normalizedLoops(x) match {
    case x: tpd.Block => Some(x)
    case _ => None
  }

  /** Normalizes non Blocks.
   *  i) Put `while` loops in their own blocks: `{ def while$() = ...; while$() }`
   *  ii) Put closures in their own blocks: `{ def anon$() = ...; closure(anon$, ...) }`
   */
  private def normalizedLoops(tree: tpd.Tree)(given Context): tpd.Tree = tree match {
    case block: tpd.Block if block.stats.size > 1 =>
      def normalizeInnerLoops(stats: List[tpd.Tree]): List[tpd.Tree] = stats match {
        case (x: tpd.DefDef) :: y :: xs if needsNormalization(y) =>
          tpd.Block(x :: Nil, y) :: normalizeInnerLoops(xs)
        case x :: xs => x :: normalizeInnerLoops(xs)
        case Nil => Nil
      }
      if (needsNormalization(block.expr)) {
        val stats1 = normalizeInnerLoops(block.stats.init)
        val normalLoop = tpd.Block(block.stats.last :: Nil, block.expr)
        tpd.Block(stats1, normalLoop)
      }
      else {
        val stats1 = normalizeInnerLoops(block.stats)
        tpd.cpy.Block(block)(stats1, block.expr)
      }
    case _ => tree
  }

  /** If it is the second statement of a closure. See: `normalizedLoops` */
  private def needsNormalization(tree: tpd.Tree)(given Context): Boolean = tree match {
    case _: tpd.Closure => true
    case _ => false
  }

  def Block_statements(self: Block)(given Context): List[Statement] = self.stats
  def Block_expr(self: Block)(given Context): Term = self.expr

  def Block_apply(stats: List[Statement], expr: Term)(given Context): Block =
    withDefaultPos(tpd.Block(stats, expr))

  def Block_copy(original: Tree)(stats: List[Statement], expr: Term)(given Context): Block =
    tpd.cpy.Block(original)(stats, expr)

  type Inlined = tpd.Inlined

  def matchInlined(x: Term)(given Context): Option[Inlined] = x match {
    case x: tpd.Inlined => Some(x)
    case _ => None
  }

  def Inlined_call(self: Inlined)(given Context): Option[Term | TypeTree] = optional(self.call)
  def Inlined_bindings(self: Inlined)(given Context): List[Definition] = self.bindings
  def Inlined_body(self: Inlined)(given Context): Term = self.expansion

  def Inlined_apply(call: Option[Term | TypeTree], bindings: List[Definition], expansion: Term)(given Context): Inlined =
    withDefaultPos(tpd.Inlined(call.getOrElse(tpd.EmptyTree), bindings.map { case b: tpd.MemberDef => b }, expansion))

  def Inlined_copy(original: Tree)(call: Option[Term | TypeTree], bindings: List[Definition], expansion: Term)(given Context): Inlined =
    tpd.cpy.Inlined(original)(call.getOrElse(tpd.EmptyTree), bindings.asInstanceOf[List[tpd.MemberDef]], expansion)

  type Closure = tpd.Closure

  def matchClosure(x: Term)(given Context): Option[Closure] = x match {
    case x: tpd.Closure => Some(x)
    case _ => None
  }

  def Closure_meth(self: Closure)(given Context): Term = self.meth
  def Closure_tpeOpt(self: Closure)(given Context): Option[Type] = optional(self.tpt).map(_.tpe)

  def Closure_apply(meth: Term, tpe: Option[Type])(given Context): Closure =
    withDefaultPos(tpd.Closure(Nil, meth, tpe.map(tpd.TypeTree(_)).getOrElse(tpd.EmptyTree)))

  def Closure_copy(original: Tree)(meth: Tree, tpe: Option[Type])(given Context): Closure =
    tpd.cpy.Closure(original)(Nil, meth, tpe.map(tpd.TypeTree(_)).getOrElse(tpd.EmptyTree))

  type If = tpd.If

  def matchIf(x: Term)(given Context): Option[If] = x match {
    case x: tpd.If => Some(x)
    case _ => None
  }

  def If_cond(self: If)(given Context): Term = self.cond
  def If_thenp(self: If)(given Context): Term = self.thenp
  def If_elsep(self: If)(given Context): Term = self.elsep

  def If_apply(cond: Term, thenp: Term, elsep: Term)(given Context): If =
    withDefaultPos(tpd.If(cond, thenp, elsep))

  def If_copy(original: Tree)(cond: Term, thenp: Term, elsep: Term)(given Context): If =
    tpd.cpy.If(original)(cond, thenp, elsep)

  type Match = tpd.Match

  def matchMatch(x: Term)(given Context): Option[Match] = x match {
    case x: tpd.Match if !x.selector.isEmpty => Some(x)
    case _ => None
  }

  def Match_scrutinee(self: Match)(given Context): Term = self.selector
  def Match_cases(self: Match)(given Context): List[CaseDef] = self.cases

  def Match_apply(selector: Term, cases: List[CaseDef])(given Context): Match =
    withDefaultPos(tpd.Match(selector, cases))

  def Match_copy(original: Tree)(selector: Term, cases: List[CaseDef])(given Context): Match =
    tpd.cpy.Match(original)(selector, cases)

  type ImpliedMatch = tpd.Match

  def matchImplicitMatch(x: Term)(given Context): Option[Match] = x match {
    case x: tpd.Match if x.selector.isEmpty => Some(x)
    case _ => None
  }

  def ImplicitMatch_cases(self: Match)(given Context): List[CaseDef] = self.cases

  def ImplicitMatch_apply(cases: List[CaseDef])(given Context): ImpliedMatch =
    withDefaultPos(tpd.Match(tpd.EmptyTree, cases))

  def ImplicitMatch_copy(original: Tree)(cases: List[CaseDef])(given Context): ImpliedMatch =
    tpd.cpy.Match(original)(tpd.EmptyTree, cases)

  type Try = tpd.Try

  def matchTry(x: Term)(given Context): Option[Try] = x match {
    case x: tpd.Try => Some(x)
    case _ => None
  }

  def Try_body(self: Try)(given Context): Term = self.expr
  def Try_cases(self: Try)(given Context): List[CaseDef] = self.cases
  def Try_finalizer(self: Try)(given Context): Option[Term] = optional(self.finalizer)

  def Try_apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(given Context): Try =
    withDefaultPos(tpd.Try(expr, cases, finalizer.getOrElse(tpd.EmptyTree)))

  def Try_copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(given Context): Try =
    tpd.cpy.Try(original)(expr, cases, finalizer.getOrElse(tpd.EmptyTree))

  type Return = tpd.Return

  def matchReturn(x: Term)(given Context): Option[Return] = x match {
    case x: tpd.Return => Some(x)
    case _ => None
  }

  def Return_expr(self: Return)(given Context): Term = self.expr

  def Return_apply(expr: Term)(given ctx: Context): Return =
    withDefaultPos(tpd.Return(expr, ctx.owner))

  def Return_copy(original: Tree)(expr: Term)(given ctx: Context): Return =
    tpd.cpy.Return(original)(expr, tpd.ref(ctx.owner))

  type Repeated = tpd.SeqLiteral

  def matchRepeated(x: Term)(given Context): Option[Repeated] = x match {
    case x: tpd.SeqLiteral => Some(x)
    case _ => None
  }

  def Repeated_elems(self: Repeated)(given Context): List[Term] = self.elems
  def Repeated_elemtpt(self: Repeated)(given Context): TypeTree = self.elemtpt

  def Repeated_apply(elems: List[Term], elemtpt: TypeTree)(given Context): Repeated =
    withDefaultPos(tpd.SeqLiteral(elems, elemtpt))

  def Repeated_copy(original: Tree)(elems: List[Term], elemtpt: TypeTree)(given Context): Repeated =
    tpd.cpy.SeqLiteral(original)(elems, elemtpt)

  type SelectOuter = tpd.Select

  def matchSelectOuter(x: Term)(given Context): Option[SelectOuter] = x match {
    case x: tpd.Select =>
      x.name match {
        case NameKinds.OuterSelectName(_, _) => Some(x)
        case _ => None
      }
    case _ => None
  }

  def SelectOuter_qualifier(self: SelectOuter)(given Context): Term = self.qualifier
  def SelectOuter_level(self: SelectOuter)(given Context): Int = {
    val NameKinds.OuterSelectName(_, levels) = self.name
    levels
  }
  def SelectOuter_tpe(self: SelectOuter)(given Context): Type = self.tpe.stripTypeVar

  def SelectOuter_apply(qualifier: Term, name: String, levels: Int)(given Context): SelectOuter =
    withDefaultPos(tpd.Select(qualifier, NameKinds.OuterSelectName(name.toTermName, levels)))

  def SelectOuter_copy(original: Tree)(qualifier: Term, name: String, levels: Int)(given Context): SelectOuter =
    tpd.cpy.Select(original)(qualifier, NameKinds.OuterSelectName(name.toTermName, levels))

  type While = tpd.WhileDo

  def matchWhile(x: Term)(given Context): Option[While] = x match {
    case x: tpd.WhileDo => Some(x)
    case _ => None
  }

  def While_cond(self: While)(given Context): Term = self.cond
  def While_body(self: While)(given Context): Term = self.body

  def While_apply(cond: Term, body: Term)(given Context): While =
    withDefaultPos(tpd.WhileDo(cond, body))

  def While_copy(original: Tree)(cond: Term, body: Term)(given Context): While =
    tpd.cpy.WhileDo(original)(cond, body)

  type TypeTree = tpd.Tree

  def matchTypeTree(x: TypeTree | TypeBoundsTree)(given Context): Option[TypeTree] = x match {
    case x: tpd.TypeBoundsTree => None
    case _ => if (x.isType) Some(x) else None
  }

  def TypeTree_pos(self: TypeTree)(given Context): Position = self.sourcePos
  def TypeTree_symbol(self: TypeTree)(given Context): Symbol = self.symbol
  def TypeTree_tpe(self: TypeTree)(given Context): Type = self.tpe.stripTypeVar

  type Inferred = tpd.TypeTree

  def matchInferred(tpt: TypeTree | TypeBoundsTree)(given Context): Option[Inferred] = tpt match {
    case tpt: tpd.TypeTree if !tpt.tpe.isInstanceOf[Types.TypeBounds] => Some(tpt)
    case _ => None
  }

  def Inferred_apply(tpe: Type)(given Context): Inferred = withDefaultPos(tpd.TypeTree(tpe))

  type TypeIdent = tpd.Ident

  def matchTypeIdent(tpt: TypeTree | TypeBoundsTree)(given Context): Option[TypeIdent] = tpt match {
    case tpt: tpd.Ident if tpt.isType => Some(tpt)
    case _ => None
  }

  def TypeIdent_name(self: TypeIdent)(given Context): String = self.name.toString

  def TypeIdent_copy(original: TypeIdent)(name: String)(given Context): TypeIdent =
    tpd.cpy.Ident(original)(name.toTypeName)

  type TypeSelect = tpd.Select

  def matchTypeSelect(tpt: TypeTree | TypeBoundsTree)(given Context): Option[TypeSelect] = tpt match {
    case tpt: tpd.Select if tpt.isType && tpt.qualifier.isTerm  => Some(tpt)
    case _ => None
  }

  def TypeSelect_qualifier(self: TypeSelect)(given Context): Term = self.qualifier
  def TypeSelect_name(self: TypeSelect)(given Context): String = self.name.toString

  def TypeSelect_apply(qualifier: Term, name: String)(given Context): TypeSelect =
    withDefaultPos(tpd.Select(qualifier, name.toTypeName))

  def TypeSelect_copy(original: TypeSelect)(qualifier: Term, name: String)(given Context): TypeSelect =
    tpd.cpy.Select(original)(qualifier, name.toTypeName)


  type Projection = tpd.Select

  def matchProjection(tpt: TypeTree | TypeBoundsTree)(given Context): Option[Projection] = tpt match {
    case tpt: tpd.Select if tpt.isType && tpt.qualifier.isType => Some(tpt)
    case _ => None
  }

  def Projection_qualifier(self: Projection)(given Context): TypeTree = self.qualifier
  def Projection_name(self: Projection)(given Context): String = self.name.toString

  def Projection_copy(original: Projection)(qualifier: TypeTree, name: String)(given Context): Projection =
    tpd.cpy.Select(original)(qualifier, name.toTypeName)

  type Singleton = tpd.SingletonTypeTree

  def matchSingleton(tpt: TypeTree | TypeBoundsTree)(given Context): Option[Singleton] = tpt match {
    case tpt: tpd.SingletonTypeTree => Some(tpt)
    case _ => None
  }

  def Singleton_ref(self: Singleton)(given Context): Term = self.ref

  def Singleton_apply(ref: Term)(given Context): Singleton =
    withDefaultPos(tpd.SingletonTypeTree(ref))

  def Singleton_copy(original: Singleton)(ref: Term)(given Context): Singleton =
    tpd.cpy.SingletonTypeTree(original)(ref)

  type Refined = tpd.RefinedTypeTree

  def matchRefined(tpt: TypeTree | TypeBoundsTree)(given Context): Option[Refined] = tpt match {
    case tpt: tpd.RefinedTypeTree => Some(tpt)
    case _ => None
  }

  def Refined_tpt(self: Refined)(given Context): TypeTree = self.tpt
  def Refined_refinements(self: Refined)(given Context): List[Definition] = self.refinements

  def Refined_copy(original: Refined)(tpt: TypeTree, refinements: List[Definition])(given Context): Refined =
    tpd.cpy.RefinedTypeTree(original)(tpt, refinements)

  type Applied = tpd.AppliedTypeTree

  def matchApplied(tpt: TypeTree | TypeBoundsTree)(given Context): Option[Applied] = tpt match {
    case tpt: tpd.AppliedTypeTree => Some(tpt)
    case _ => None
  }

  def Applied_tpt(self: Applied)(given Context): TypeTree = self.tpt
  def Applied_args(self: Applied)(given Context): List[TypeTree | TypeBoundsTree] = self.args

  def Applied_apply(tpt: TypeTree, args: List[TypeTree | TypeBoundsTree])(given Context): Applied =
    withDefaultPos(tpd.AppliedTypeTree(tpt, args))

  def Applied_copy(original: Applied)(tpt: TypeTree, args: List[TypeTree | TypeBoundsTree])(given Context): Applied =
    tpd.cpy.AppliedTypeTree(original)(tpt, args)

  type Annotated = tpd.Annotated

  def matchAnnotated(tpt: TypeTree | TypeBoundsTree)(given Context): Option[Annotated] = tpt match {
    case tpt: tpd.Annotated => Some(tpt)
    case _ => None
  }

  def Annotated_arg(self: Annotated)(given Context): TypeTree = self.arg
  def Annotated_annotation(self: Annotated)(given Context): Term = self.annot

  def Annotated_apply(arg: TypeTree, annotation: Term)(given Context): Annotated =
    withDefaultPos(tpd.Annotated(arg, annotation))

  def Annotated_copy(original: Annotated)(arg: TypeTree, annotation: Term)(given Context): Annotated =
    tpd.cpy.Annotated(original)(arg, annotation)

  type MatchTypeTree = tpd.MatchTypeTree

  def matchMatchTypeTree(tpt: TypeTree | TypeBoundsTree)(given Context): Option[MatchTypeTree] = tpt match {
    case tpt: tpd.MatchTypeTree => Some(tpt)
    case _ => None
  }

  def MatchTypeTree_bound(self: MatchTypeTree)(given Context): Option[TypeTree] = if (self.bound == tpd.EmptyTree) None else Some(self.bound)
  def MatchTypeTree_selector(self: MatchTypeTree)(given Context): TypeTree = self.selector
  def MatchTypeTree_cases(self: MatchTypeTree)(given Context): List[CaseDef] = self.cases

  def MatchTypeTree_apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(given Context): MatchTypeTree =
    withDefaultPos(tpd.MatchTypeTree(bound.getOrElse(tpd.EmptyTree), selector, cases))

  def MatchTypeTree_copy(original: MatchTypeTree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef])(given Context): MatchTypeTree =
    tpd.cpy.MatchTypeTree(original)(bound.getOrElse(tpd.EmptyTree), selector, cases)

  type ByName = tpd.ByNameTypeTree

  def matchByName(tpt: TypeTree | TypeBoundsTree)(given Context): Option[ByName] = tpt match {
    case tpt: tpd.ByNameTypeTree => Some(tpt)
    case _ => None
  }

  def ByName_result(self: ByName)(given Context): TypeTree = self.result

  def ByName_apply(result: TypeTree)(given Context): ByName =
    withDefaultPos(tpd.ByNameTypeTree(result))

  def ByName_copy(original: ByName)(result: TypeTree)(given Context): ByName =
    tpd.cpy.ByNameTypeTree(original)(result)

  type LambdaTypeTree = tpd.LambdaTypeTree

  def matchLambdaTypeTree(tpt: TypeTree | TypeBoundsTree)(given Context): Option[LambdaTypeTree] = tpt match {
    case tpt: tpd.LambdaTypeTree => Some(tpt)
    case _ => None
  }

  def Lambdatparams(self: LambdaTypeTree)(given Context): List[TypeDef] = self.tparams
  def Lambdabody(self: LambdaTypeTree)(given Context): TypeTree | TypeBoundsTree = self.body

  def Lambdaapply(tparams: List[TypeDef], body: TypeTree | TypeBoundsTree)(given Context): LambdaTypeTree =
    withDefaultPos(tpd.LambdaTypeTree(tparams, body))

  def Lambdacopy(original: LambdaTypeTree)(tparams: List[TypeDef], body: TypeTree | TypeBoundsTree)(given Context): LambdaTypeTree =
    tpd.cpy.LambdaTypeTree(original)(tparams, body)

  type TypeBind = tpd.Bind

  def matchTypeBind(tpt: TypeTree | TypeBoundsTree)(given Context): Option[TypeBind] = tpt match {
    case tpt: tpd.Bind if tpt.name.isTypeName => Some(tpt)
    case _ => None
  }

  def TypeBind_name(self: TypeBind)(given Context): String = self.name.toString
  def TypeBind_body(self: TypeBind)(given Context): TypeTree | TypeBoundsTree = self.body

  def TypeBind_copy(original: TypeBind)(name: String, tpt: TypeTree | TypeBoundsTree)(given Context): TypeBind =
    tpd.cpy.Bind(original)(name.toTypeName, tpt)

  type TypeBlock = tpd.Block

  def matchTypeBlock(tpt: TypeTree | TypeBoundsTree)(given Context): Option[TypeBlock] = tpt match {
    case tpt: tpd.Block => Some(tpt)
    case _ => None
  }

  def TypeBlock_aliases(self: TypeBlock)(given Context): List[TypeDef] = self.stats.map { case alias: TypeDef => alias }
  def TypeBlock_tpt(self: TypeBlock)(given Context): TypeTree = self.expr

  def TypeBlock_apply(aliases: List[TypeDef], tpt: TypeTree)(given Context): TypeBlock =
    withDefaultPos(tpd.Block(aliases, tpt))

  def TypeBlock_copy(original: TypeBlock)(aliases: List[TypeDef], tpt: TypeTree)(given Context): TypeBlock =
    tpd.cpy.Block(original)(aliases, tpt)

  type TypeBoundsTree = tpd.TypeBoundsTree

  def matchTypeBoundsTree(x: TypeTree | TypeBoundsTree)(given Context): Option[TypeBoundsTree] = x match {
    case x: tpd.TypeBoundsTree => Some(x)
    case x @ Trees.TypeTree() =>
      // TODO only enums generate this kind of type bounds. Is this possible without enums? If not generate tpd.TypeBoundsTree for enums instead
      x.tpe match {
        case tpe: Types.TypeBounds =>
          Some(tpd.TypeBoundsTree(tpd.TypeTree(tpe.lo).withSpan(x.span), tpd.TypeTree(tpe.hi).withSpan(x.span)))
        case _ => None
      }
    case _ => None
  }

  def TypeBoundsTree_tpe(self: TypeBoundsTree)(given Context): TypeBounds = self.tpe.asInstanceOf[Types.TypeBounds]
  def TypeBoundsTree_low(self: TypeBoundsTree)(given Context): TypeTree = self.lo
  def TypeBoundsTree_hi(self: TypeBoundsTree)(given Context): TypeTree = self.hi

  type WildcardTypeTree = tpd.Ident

  def matchWildcardTypeTree(x: TypeTree | TypeBoundsTree)(given Context): Option[WildcardTypeTree] = x match {
    case x @ Trees.Ident(nme.WILDCARD) => Some(x)
    case _ => None
  }

  def WildcardTypeTree_tpe(self: WildcardTypeTree)(given Context): TypeOrBounds = self.tpe.stripTypeVar

  type CaseDef = tpd.CaseDef

  def matchCaseDef(tree: Tree)(given Context): Option[CaseDef] = tree match {
    case tree: tpd.CaseDef if tree.body.isTerm => Some(tree)
    case _ => None
  }

  def CaseDef_pattern(self: CaseDef)(given Context): Pattern = self.pat
  def CaseDef_guard(self: CaseDef)(given Context): Option[Term] = optional(self.guard)
  def CaseDef_rhs(self: CaseDef)(given Context): Term = self.body

  def CaseDef_module_apply(pattern: Pattern, guard: Option[Term], body: Term)(given Context): CaseDef =
    tpd.CaseDef(pattern, guard.getOrElse(tpd.EmptyTree), body)

  def CaseDef_module_copy(original: CaseDef)(pattern: Pattern, guard: Option[Term], body: Term)(given Context): CaseDef =
    tpd.cpy.CaseDef(original)(pattern, guard.getOrElse(tpd.EmptyTree), body)

  type TypeCaseDef = tpd.CaseDef

  def matchTypeCaseDef(tree: Tree)(given Context): Option[TypeCaseDef] = tree match {
    case tree: tpd.CaseDef if tree.body.isType => Some(tree)
    case _ => None
  }

  def TypeCaseDef_pattern(self: TypeCaseDef)(given Context): TypeTree = self.pat
  def TypeCaseDef_rhs(self: TypeCaseDef)(given Context): TypeTree = self.body

  def TypeCaseDef_module_apply(pattern: TypeTree, body: TypeTree)(given Context): TypeCaseDef =
    tpd.CaseDef(pattern, tpd.EmptyTree, body)

  def TypeCaseDef_module_copy(original: TypeCaseDef)(pattern: TypeTree, body: TypeTree)(given Context): TypeCaseDef =
    tpd.cpy.CaseDef(original)(pattern, tpd.EmptyTree, body)

  //
  // PATTERNS
  //

  type Pattern = tpd.Tree

  def Pattern_pos(self: Pattern)(given Context): Position = self.sourcePos
  def Pattern_tpe(self: Pattern)(given Context): Type = self.tpe.stripTypeVar
  def Pattern_symbol(self: Pattern)(given Context): Symbol = self.symbol

  type Value = tpd.Tree

  def matchPattern_Value(pattern: Pattern): Option[Value] = pattern match {
    case lit: tpd.Literal => Some(lit)
    case ref: tpd.RefTree if ref.isTerm && !tpd.isWildcardArg(ref) => Some(ref)
    case ths: tpd.This => Some(ths)
    case _ => None
  }

  def Pattern_Value_value(self: Value)(given Context): Term = self

  def Pattern_Value_module_apply(term: Term)(given Context): Value = term match {
    case lit: tpd.Literal => lit
    case ref: tpd.RefTree if ref.isTerm => ref
    case ths: tpd.This => ths
  }
  def Pattern_Value_module_copy(original: Value)(term: Term)(given Context): Value = term match {
    case lit: tpd.Literal => tpd.cpy.Literal(original)(lit.const)
    case ref: tpd.RefTree if ref.isTerm => tpd.cpy.Ref(original.asInstanceOf[tpd.RefTree])(ref.name)
    case ths: tpd.This => tpd.cpy.This(original)(ths.qual)
  }

  type Bind = tpd.Bind

  def matchPattern_Bind(x: Pattern)(given Context): Option[Bind] = x match {
    case x: tpd.Bind if x.name.isTermName => Some(x)
    case _ => None
  }

  def Pattern_Bind_name(self: Bind)(given Context): String = self.name.toString

  def Pattern_Bind_pattern(self: Bind)(given Context): Pattern = self.body

  def Pattern_Bind_module_copy(original: Bind)(name: String, pattern: Pattern)(given Context): Bind =
    withDefaultPos(tpd.cpy.Bind(original)(name.toTermName, pattern))

  type Unapply = tpd.UnApply

  def matchPattern_Unapply(pattern: Pattern)(given Context): Option[Unapply] = pattern match {
    case pattern @ Trees.UnApply(_, _, _) => Some(pattern)
    case Trees.Typed(pattern @ Trees.UnApply(_, _, _), _) => Some(pattern)
    case _ => None
  }

  def Pattern_Unapply_fun(self: Unapply)(given Context): Term = self.fun
  def Pattern_Unapply_implicits(self: Unapply)(given Context): List[Term] = self.implicits
  def Pattern_Unapply_patterns(self: Unapply)(given Context): List[Pattern] = effectivePatterns(self.patterns)

  def Pattern_Unapply_module_copy(original: Unapply)(fun: Term, implicits: List[Term], patterns: List[Pattern])(given Context): Unapply =
    withDefaultPos(tpd.cpy.UnApply(original)(fun, implicits, patterns))

  private def effectivePatterns(patterns: List[Pattern]): List[Pattern] = patterns match {
    case patterns0 :+ Trees.SeqLiteral(elems, _) => patterns0 ::: elems
    case _ => patterns
  }

  type Alternatives = tpd.Alternative

  def matchPattern_Alternatives(pattern: Pattern)(given Context): Option[Alternatives] = pattern match {
    case pattern: tpd.Alternative => Some(pattern)
    case _ => None
  }

  def Pattern_Alternatives_patterns(self: Alternatives)(given Context): List[Pattern] = self.trees

  def Pattern_Alternatives_module_apply(patterns: List[Pattern])(given Context): Alternatives =
    withDefaultPos(tpd.Alternative(patterns))

  def Pattern_Alternatives_module_copy(original: Alternatives)(patterns: List[Pattern])(given Context): Alternatives =
    tpd.cpy.Alternative(original)(patterns)

  type TypeTest = tpd.Typed

  def matchPattern_TypeTest(pattern: Pattern)(given Context): Option[TypeTest] = pattern match {
    case Trees.Typed(_: tpd.UnApply, _) => None
    case pattern: tpd.Typed => Some(pattern)
    case _ => None
  }

  def Pattern_TypeTest_tpt(self: TypeTest)(given Context): TypeTree = self.tpt

  def Pattern_TypeTest_module_apply(tpt: TypeTree)(given ctx: Context): TypeTest =
    withDefaultPos(tpd.Typed(untpd.Ident(nme.WILDCARD)(ctx.source).withType(tpt.tpe), tpt))

  def Pattern_TypeTest_module_copy(original: TypeTest)(tpt: TypeTree)(given Context): TypeTest =
    tpd.cpy.Typed(original)(untpd.Ident(nme.WILDCARD).withSpan(original.span).withType(tpt.tpe), tpt)

  type WildcardPattern = tpd.Ident

  def matchPattern_WildcardPattern(pattern: Pattern)(given Context): Option[WildcardPattern] =
    pattern match {
      case pattern: tpd.Ident if tpd.isWildcardArg(pattern) => Some(pattern)
      case _ => None
    }

  def Pattern_WildcardPattern_module_apply(tpe: TypeOrBounds)(given Context): WildcardPattern =
    untpd.Ident(nme.WILDCARD).withType(tpe)

  //
  // TYPES
  //

  type TypeOrBounds = Types.Type

  type NoPrefix = Types.NoPrefix.type

  def matchNoPrefix(x: TypeOrBounds)(given Context): Option[NoPrefix] =
    if (x == Types.NoPrefix) Some(Types.NoPrefix) else None

  type TypeBounds = Types.TypeBounds

  def matchTypeBounds(x: TypeOrBounds)(given Context): Option[TypeBounds] = x match {
    case x: Types.TypeBounds => Some(x)
    case _ => None
  }

  def TypeBounds_low(self: TypeBounds)(given Context): Type = self.lo
  def TypeBounds_hi(self: TypeBounds)(given Context): Type = self.hi

  type Type = Types.Type

  def matchType(x: TypeOrBounds)(given Context): Option[Type] = x match {
    case x: TypeBounds => None
    case x if x == Types.NoPrefix => None
    case _ => Some(x)
  }

  def Type_apply(clazz: Class[?])(given ctx: Context): Type =
    if (clazz.isPrimitive)
      if (clazz == classOf[Boolean]) defn.BooleanType
      else if (clazz == classOf[Byte]) defn.ByteType
      else if (clazz == classOf[Char]) defn.CharType
      else if (clazz == classOf[Short]) defn.ShortType
      else if (clazz == classOf[Int]) defn.IntType
      else if (clazz == classOf[Long]) defn.LongType
      else if (clazz == classOf[Float]) defn.FloatType
      else if (clazz == classOf[Double]) defn.DoubleType
      else defn.UnitType
    else if (clazz.isArray)
      defn.ArrayType.appliedTo(Type_apply(clazz.getComponentType))
    else if (clazz.isMemberClass) {
      val name = clazz.getSimpleName.toTypeName
      val enclosing = Type_apply(clazz.getEnclosingClass)
      if (enclosing.member(name).exists) enclosing.select(name)
      else
        enclosing.classSymbol.companionModule.termRef.select(name)
    }
    else ctx.getClassIfDefined(clazz.getCanonicalName).typeRef

  def `Type_=:=`(self: Type)(that: Type)(given Context): Boolean = self =:= that

  def `Type_<:<`(self: Type)(that: Type)(given Context): Boolean = self <:< that

  /** Widen from singleton type to its underlying non-singleton
    *  base type by applying one or more `underlying` dereferences,
    *  Also go from => T to T.
    *  Identity for all other types. Example:
    *
    *  class Outer { class C ; val x: C }
    *  def o: Outer
    *  <o.x.type>.widen = o.C
    */
  def Type_widen(self: Type)(given Context): Type = self.widen

  def Type_dealias(self: Type)(given Context): Type = self.dealias

  def Type_simplified(self: Type)(given Context): Type = self.simplified

  def Type_classSymbol(self: Type)(given Context): Option[Symbol] =
    if (self.classSymbol.exists) Some(self.classSymbol.asClass) else None

  def Type_typeSymbol(self: Type)(given Context): Symbol = self.typeSymbol

  def Type_termSymbol(self: Type)(given Context): Symbol = self.termSymbol

  def Type_isSingleton(self: Type)(given Context): Boolean = self.isSingleton

  def Type_memberType(self: Type)(member: Symbol)(given Context): Type =
    member.info.asSeenFrom(self, member.owner)

  def Type_derivesFrom(self: Type)(cls: Symbol)(given Context): Boolean =
    self.derivesFrom(cls)

  def Type_isFunctionType(self: Type)(given Context): Boolean =
    defn.isFunctionType(self)

  def Type_isImplicitFunctionType(self: Type)(given Context): Boolean =
    defn.isImplicitFunctionType(self)

  def Type_isErasedFunctionType(self: Type)(given Context): Boolean =
    defn.isErasedFunctionType(self)

  def Type_isDependentFunctionType(self: Type)(given Context): Boolean = {
    val tpNoRefinement = self.dropDependentRefinement
    tpNoRefinement != self && defn.isNonRefinedFunction(tpNoRefinement)
  }

  type ConstantType = Types.ConstantType

  def matchConstantType(tpe: TypeOrBounds)(given Context): Option[ConstantType] = tpe match {
    case tpe: Types.ConstantType => Some(tpe)
    case _ => None
  }

  def ConstantType_constant(self: ConstantType)(given Context): Constant = self.value

  type TermRef = Types.NamedType

  def matchTermRef(tpe: TypeOrBounds)(given Context): Option[TermRef] = tpe match {
    case tp: Types.TermRef => Some(tp)
    case _ => None
  }

  def TermRef_apply(qual: TypeOrBounds, name: String)(given Context): TermRef =
    Types.TermRef(qual, name.toTermName)

  def TermRef_qualifier(self: TermRef)(given Context): TypeOrBounds = self.prefix

  def TermRef_name(self: TermRef)(given Context): String = self.name.toString

  type TypeRef = Types.NamedType

  def matchTypeRef(tpe: TypeOrBounds)(given Context): Option[TypeRef] = tpe match {
    case tp: Types.TypeRef => Some(tp)
    case _ => None
  }

  def TypeRef_qualifier(self: TypeRef)(given Context): TypeOrBounds = self.prefix

  def TypeRef_name(self: TypeRef)(given Context): String = self.name.toString

  type NamedTermRef = Types.NamedType

  def matchNamedTermRef(tpe: TypeOrBounds)(given Context): Option[NamedTermRef] = tpe match {
    case tpe: Types.NamedType =>
      tpe.designator match {
        case name: Names.TermName => Some(tpe)
        case _ => None
      }
    case _ => None
  }

  def NamedTermRef_name(self: NamedTermRef)(given Context): String = self.name.toString
  def NamedTermRef_qualifier(self: NamedTermRef)(given Context): TypeOrBounds = self.prefix

  type SuperType = Types.SuperType

  def matchSuperType(tpe: TypeOrBounds)(given Context): Option[SuperType] = tpe match {
    case tpe: Types.SuperType => Some(tpe)
    case _ => None
  }

  def SuperType_thistpe(self: SuperType)(given Context): Type = self.thistpe
  def SuperType_supertpe(self: SuperType)(given Context): Type = self.supertpe

  type Refinement = Types.RefinedType

  def matchRefinement(tpe: TypeOrBounds)(given Context): Option[Refinement] = tpe match {
    case tpe: Types.RefinedType => Some(tpe)
    case _ => None
  }

  def Refinement_parent(self: Refinement)(given Context): Type = self.parent
  def Refinement_name(self: Refinement)(given Context): String = self.refinedName.toString
  def Refinement_info(self: Refinement)(given Context): TypeOrBounds = self.refinedInfo

  type AppliedType = Types.AppliedType

  def matchAppliedType(tpe: TypeOrBounds)(given Context): Option[AppliedType] = tpe match {
    case tpe: Types.AppliedType => Some(tpe)
    case _ => None
  }

  def AppliedType_tycon(self: AppliedType)(given Context): Type = self.tycon
  def AppliedType_args(self: AppliedType)(given Context): List[TypeOrBounds] = self.args

  def AppliedType_apply(tycon: Type, args: List[TypeOrBounds])(given Context): AppliedType = Types.AppliedType(tycon, args)

  type AnnotatedType = Types.AnnotatedType

  def matchAnnotatedType(tpe: TypeOrBounds)(given Context): Option[AnnotatedType] = tpe match {
    case tpe: Types.AnnotatedType => Some(tpe)
    case _ => None
  }

  def AnnotatedType_underlying(self: AnnotatedType)(given Context): Type = self.underlying.stripTypeVar
  def AnnotatedType_annot(self: AnnotatedType)(given Context): Term = self.annot.tree

  type AndType = Types.AndType

  def matchAndType(tpe: TypeOrBounds)(given Context): Option[AndType] = tpe match {
    case tpe: Types.AndType => Some(tpe)
    case _ => None
  }

  def AndType_left(self: AndType)(given Context): Type = self.tp1.stripTypeVar
  def AndType_right(self: AndType)(given Context): Type = self.tp2.stripTypeVar

  type OrType = Types.OrType

  def matchOrType(tpe: TypeOrBounds)(given Context): Option[OrType] = tpe match {
    case tpe: Types.OrType => Some(tpe)
    case _ => None
  }

  def OrType_left(self: OrType)(given Context): Type = self.tp1.stripTypeVar
  def OrType_right(self: OrType)(given Context): Type = self.tp2.stripTypeVar

  type MatchType = Types.MatchType

  def matchMatchType(tpe: TypeOrBounds)(given Context): Option[MatchType] = tpe match {
    case tpe: Types.MatchType => Some(tpe)
    case _ => None
  }

  def MatchType_bound(self: MatchType)(given Context): Type = self.bound
  def MatchType_scrutinee(self: MatchType)(given Context): Type = self.scrutinee
  def MatchType_cases(self: MatchType)(given Context): List[Type] = self.cases

  type ByNameType = Types.ExprType

  def matchByNameType(tpe: TypeOrBounds)(given Context): Option[ByNameType] = tpe match {
    case tpe: Types.ExprType => Some(tpe)
    case _ => None
  }

  def ByNameType_underlying(self: ByNameType)(given Context): Type = self.resType.stripTypeVar

  type ParamRef = Types.ParamRef

  def matchParamRef(tpe: TypeOrBounds)(given Context): Option[ParamRef] = tpe match {
    case tpe: Types.TypeParamRef => Some(tpe)
    case tpe: Types.TermParamRef => Some(tpe)
    case _ => None
  }

  def ParamRef_binder(self: ParamRef)(given Context): LambdaType[TypeOrBounds] =
    self.binder.asInstanceOf[LambdaType[TypeOrBounds]] // Cast to tpd
  def ParamRef_paramNum(self: ParamRef)(given Context): Int = self.paramNum

  type ThisType = Types.ThisType

  def matchThisType(tpe: TypeOrBounds)(given Context): Option[ThisType] = tpe match {
    case tpe: Types.ThisType => Some(tpe)
    case _ => None
  }

  def ThisType_tref(self: ThisType)(given Context): Type = self.tref

  type RecursiveThis = Types.RecThis

  def matchRecursiveThis(tpe: TypeOrBounds)(given Context): Option[RecursiveThis] = tpe match {
    case tpe: Types.RecThis => Some(tpe)
    case _ => None
  }

  def RecursiveThis_binder(self: RecursiveThis)(given Context): RecursiveType = self.binder

  type RecursiveType = Types.RecType

  def matchRecursiveType(tpe: TypeOrBounds)(given Context): Option[RecursiveType] = tpe match {
    case tpe: Types.RecType => Some(tpe)
    case _ => None
  }

  def RecursiveType_underlying(self: RecursiveType)(given Context): Type = self.underlying.stripTypeVar

  type LambdaType[ParamInfo] = Types.LambdaType { type PInfo = ParamInfo }

  type MethodType = Types.MethodType

  def matchMethodType(tpe: TypeOrBounds)(given Context): Option[MethodType] = tpe match {
    case tpe: Types.MethodType => Some(tpe)
    case _ => None
  }

  def MethodType_isErased(self: MethodType): Boolean = self.isErasedMethod
  def MethodType_isImplicit(self: MethodType): Boolean = self.isImplicitMethod
  def MethodType_paramNames(self: MethodType)(given Context): List[String] = self.paramNames.map(_.toString)
  def MethodType_paramTypes(self: MethodType)(given Context): List[Type] = self.paramInfos
  def MethodType_resType(self: MethodType)(given Context): Type = self.resType

  type PolyType = Types.PolyType

  def matchPolyType(tpe: TypeOrBounds)(given Context): Option[PolyType] = tpe match {
    case tpe: Types.PolyType => Some(tpe)
    case _ => None
  }

  def PolyType_paramNames(self: PolyType)(given Context): List[String] = self.paramNames.map(_.toString)
  def PolyType_paramBounds(self: PolyType)(given Context): List[TypeBounds] = self.paramInfos
  def PolyType_resType(self: PolyType)(given Context): Type = self.resType

  type TypeLambda = Types.TypeLambda

  def matchTypeLambda(tpe: TypeOrBounds)(given Context): Option[TypeLambda] = tpe match {
    case tpe: Types.TypeLambda => Some(tpe)
    case _ => None
  }

  def TypeLambda_paramNames(self: TypeLambda)(given Context): List[String] = self.paramNames.map(_.toString)
  def TypeLambda_paramBounds(self: TypeLambda)(given Context): List[TypeBounds] = self.paramInfos
  def TypeLambda_resType(self: TypeLambda)(given Context): Type = self.resType

  //
  // IMPORT SELECTORS
  //

  type ImportSelector = untpd.ImportSelector

  type SimpleSelector = untpd.ImportSelector

  def matchSimpleSelector(self: ImportSelector)(given Context): Option[SimpleSelector] =
    if self.renamed.isEmpty then Some(self) else None // TODO: handle import bounds

  def SimpleSelector_selection(self: SimpleSelector)(given Context): Id = self.imported

  type RenameSelector = untpd.ImportSelector

  def matchRenameSelector(self: ImportSelector)(given Context): Option[RenameSelector] =
    if self.renamed.isEmpty then None else Some(self)

  def RenameSelector_from(self: RenameSelector)(given Context): Id =
    self.imported
  def RenameSelector_to(self: RenameSelector)(given Context): Id =
    self.renamed.asInstanceOf[untpd.Ident]

  type OmitSelector = untpd.ImportSelector

  def matchOmitSelector(self: ImportSelector)(given Context): Option[OmitSelector] =
    self.renamed match
      case Trees.Ident(nme.WILDCARD) => Some(self)
      case _ => None

  def SimpleSelector_omitted(self: OmitSelector)(given Context): Id =
    self.imported

  //
  // IDENTIFIERS
  //

  type Id = untpd.Ident

  def Id_pos(self: Id)(given Context): Position = self.sourcePos

  def Id_name(self: Id)(given Context): String = self.name.toString

  //
  // SIGNATURES
  //

  type Signature = core.Signature

  def Signature_paramSigs(self: Signature): List[String | Int] =
    self.paramsSig.map {
      case paramSig: core.Names.TypeName =>
        paramSig.toString
      case paramSig: Int =>
        paramSig
    }

  def Signature_resultSig(self: Signature): String =
    self.resSig.toString

  //
  // POSITIONS
  //

  type Position = util.SourcePosition

  def Position_start(self: Position): Int = self.start

  def Position_end(self: Position): Int = self.end

  def Position_exists(self: Position): Boolean = self.exists

  def Position_sourceFile(self: Position): SourceFile = self.source

  def Position_startLine(self: Position): Int = self.startLine

  def Position_endLine(self: Position): Int = self.endLine

  def Position_startColumn(self: Position): Int = self.startColumn

  def Position_endColumn(self: Position): Int = self.endColumn

  def Position_sourceCode(self: Position): String =
    new String(self.source.content(), self.start, self.end - self.start)

  //
  // SOURCE FILES
  //

  type SourceFile = util.SourceFile

  def SourceFile_jpath(self: SourceFile): java.nio.file.Path = self.file.jpath

  def SourceFile_content(self: SourceFile): String = new String(self.content())

  //
  // COMMENTS
  //

  type Comment = core.Comments.Comment

  def Comment_raw(self: Comment): String = self.raw
  def Comment_expanded(self: Comment): Option[String] = self.expanded
  def Comment_usecases(self: Comment): List[(String, Option[DefDef])] = self.usecases.map { uc => (uc.code, uc.tpdCode) }

  //
  // CONSTANTS
  //

  type Constant = Constants.Constant

  def Constant_value(const: Constant): Any = const.value

  def matchConstant(constant: Constant): Option[Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String | Type] =
    Some(constant.value.asInstanceOf[Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String | Type])

  def matchConstant_ClassTag(x: Constant): Option[Type] =
    if (x.tag == Constants.ClazzTag) Some(x.typeValue) else None

  def Constant_apply(x: Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String | Type): Constant =
    Constants.Constant(x)

  def Constant_ClassTag_apply(x: Type): Constant = Constants.Constant(x)

  //
  // SYMBOLS
  //

  type Symbol = core.Symbols.Symbol

  def Symbol_owner(self: Symbol)(given Context): Symbol = self.owner

  def Symbol_flags(self: Symbol)(given Context): Flags = self.flags

  def Symbol_tree(self: Symbol)(given Context): Tree = {
    assert(!self.is(Case, butNot = Enum | Module))
    FromSymbol.definitionFromSym(self)
  }

  def Symbol_pattern(self: Symbol)(given ctx: Context): Pattern = {
    assert(self.is(Case, butNot = Enum | Module))
    FromSymbol.definitionFromSym(self)
  }

  def Symbol_privateWithin(self: Symbol)(given Context): Option[Type] = {
    val within = self.privateWithin
    if (within.exists && !self.is(core.Flags.Protected)) Some(within.typeRef)
    else None
  }

  def Symbol_protectedWithin(self: Symbol)(given Context): Option[Type] = {
    val within = self.privateWithin
    if (within.exists && self.is(core.Flags.Protected)) Some(within.typeRef)
    else None
  }

  def Symbol_name(self: Symbol)(given Context): String = self.name.toString

  def Symbol_fullName(self: Symbol)(given Context): String = self.fullName.toString

  def Symbol_pos(self: Symbol)(given Context): Position = self.sourcePos

  def Symbol_localContext(self: Symbol)(given ctx: Context): Context =
    if (self.exists) ctx.withOwner(self)
    else ctx

  def Symbol_comment(self: Symbol)(given ctx: Context): Option[Comment] = {
    import dotty.tools.dotc.core.Comments.CommentsContext
    val docCtx = ctx.docCtx.getOrElse {
      throw new RuntimeException(
        "DocCtx could not be found and comments are unavailable. This is a compiler-internal error."
      )
    }
    docCtx.docstring(self)
  }
  def Symbol_annots(self: Symbol)(given Context): List[Term] =
    self.annotations.flatMap {
      case _: core.Annotations.LazyBodyAnnotation => Nil
      case annot => annot.tree :: Nil
    }

  def Symbol_isDefinedInCurrentRun(self: Symbol)(given Context): Boolean =
    self.topLevelClass.asClass.isDefinedInCurrentRun

  def Symbol_isLocalDummy(self: Symbol)(given Context): Boolean = self.isLocalDummy
  def Symbol_isRefinementClass(self: Symbol)(given Context): Boolean = self.isRefinementClass
  def Symbol_isAliasType(self: Symbol)(given Context): Boolean = self.isAliasType
  def Symbol_isAnonymousClass(self: Symbol)(given Context): Boolean = self.isAnonymousClass
  def Symbol_isAnonymousFunction(self: Symbol)(given Context): Boolean = self.isAnonymousFunction
  def Symbol_isAbstractType(self: Symbol)(given Context): Boolean = self.isAbstractType
  def Symbol_isClassConstructor(self: Symbol)(given Context): Boolean = self.isClassConstructor

  def Symbol_fields(self: Symbol)(given Context): List[Symbol] =
    self.unforcedDecls.filter(isField)

  def Symbol_field(self: Symbol)(name: String)(given Context): Symbol = {
    val sym = self.unforcedDecls.find(sym => sym.name == name.toTermName)
    if (isField(sym)) sym else core.Symbols.NoSymbol
  }

  def Symbol_classMethod(self: Symbol)(name: String)(given Context): List[Symbol] =
    self.typeRef.decls.iterator.collect {
      case sym if isMethod(sym) && sym.name.toString == name => sym.asTerm
    }.toList

  def Symbol_classMethods(self: Symbol)(given Context): List[Symbol] =
    self.typeRef.decls.iterator.collect {
      case sym if isMethod(sym) => sym.asTerm
    }.toList

  private def appliedTypeRef(sym: Symbol): Type = sym.typeRef.appliedTo(sym.typeParams.map(_.typeRef))

  def Symbol_method(self: Symbol)(name: String)(given Context): List[Symbol] =
    appliedTypeRef(self).allMembers.iterator.map(_.symbol).collect {
      case sym if isMethod(sym) && sym.name.toString == name => sym.asTerm
    }.toList

  def Symbol_methods(self: Symbol)(given Context): List[Symbol] =
    appliedTypeRef(self).allMembers.iterator.map(_.symbol).collect {
      case sym if isMethod(sym) => sym.asTerm
    }.toList

  private def isMethod(sym: Symbol)(given Context): Boolean =
    sym.isTerm && sym.is(Flags.Method) && !sym.isConstructor

  def Symbol_caseFields(self: Symbol)(given Context): List[Symbol] =
    if (!self.isClass) Nil
    else self.asClass.paramAccessors.collect {
      case sym if sym.is(Flags.CaseAccessor) => sym.asTerm
    }

  private def isField(sym: Symbol)(given Context): Boolean = sym.isTerm && !sym.is(Flags.Method)

  def Symbol_of(fullName: String)(given ctx: Context): Symbol =
    ctx.requiredClass(fullName)

  def Symbol_isTypeParam(self: Symbol)(given Context): Boolean =
    self.isTypeParam

  def Symbol_isType(symbol: Symbol)(given Context): Boolean =
    symbol.isType

  def Symbol_isTerm(symbol: Symbol)(given Context): Boolean =
    symbol.isTerm

  def Symbol_isPackageDef(symbol: Symbol)(given ctx: Context): Boolean =
    symbol.is(Flags.Package)

  def Symbol_isClassDef(symbol: Symbol)(given Context): Boolean =
    symbol.isClass

  def Symbol_isTypeDef(symbol: Symbol)(given ctx: Context): Boolean =
    symbol.isType && !symbol.isClass && !symbol.is(Flags.Case)

  def Symbol_isValDef(symbol: Symbol)(given Context): Boolean =
    symbol.isTerm && !symbol.is(core.Flags.Method) && !symbol.is(core.Flags.Case/*, FIXME add this check and fix sourcecode butNot = Enum | Module*/)

  def Symbol_isDefDef(symbol: Symbol)(given Context): Boolean =
    symbol.is(core.Flags.Method)

  def Symbol_isBind(symbol: Symbol)(given Context): Boolean =
    symbol.is(core.Flags.Case, butNot = Enum | Module) && !symbol.isClass

  def Symbol_signature(self: Symbol)(given Context): Signature =
    self.signature


  def Symbol_moduleClass(self: Symbol)(given Context): Symbol = self.moduleClass

  def Symbol_companionClass(self: Symbol)(given Context): Symbol = self.companionClass

  def Symbol_companionModule(self: Symbol)(given Context): Symbol = self.companionModule

  def Symbol_noSymbol(given ctx: Context): Symbol = core.Symbols.NoSymbol

  //
  // FLAGS
  //

  type Flags = core.Flags.FlagSet

  /** Is the given flag set a subset of this flag sets */
  def Flags_is(self: Flags)(that: Flags): Boolean = self.isAllOf(that)

  /** Union of the two flag sets */
  def Flags_or(self: Flags)(that: Flags): Flags = self | that

  /** Intersection of the two flag sets */
  def Flags_and(self: Flags)(that: Flags): Flags = self & that

  def Flags_EmptyFlags: Flags = core.Flags.EmptyFlags
  def Flags_Private: Flags = core.Flags.Private
  def Flags_Protected: Flags = core.Flags.Protected
  def Flags_Abstract: Flags = core.Flags.Abstract
  def Flags_Final: Flags = core.Flags.Final
  def Flags_Sealed: Flags = core.Flags.Sealed
  def Flags_Case: Flags = core.Flags.Case
  def Flags_Implicit: Flags = core.Flags.Implicit
  def Flags_Given: Flags = core.Flags.Given
  def Flags_Erased: Flags = core.Flags.Erased
  def Flags_Lazy: Flags = core.Flags.Lazy
  def Flags_Override: Flags = core.Flags.Override
  def Flags_Inline: Flags = core.Flags.Inline
  def Flags_Macro: Flags = core.Flags.Macro
  def Flags_Static: Flags = core.Flags.JavaStatic
  def Flags_JavaDefined: Flags = core.Flags.JavaDefined
  def Flags_Object: Flags = core.Flags.Module
  def Flags_Trait: Flags = core.Flags.Trait
  def Flags_Local: Flags = core.Flags.Local
  def Flags_Synthetic: Flags = core.Flags.Synthetic
  def Flags_Artifact: Flags = core.Flags.Artifact
  def Flags_Mutable: Flags = core.Flags.Mutable
  def Flags_FieldAccessor: Flags = core.Flags.Accessor
  def Flags_CaseAcessor: Flags = core.Flags.CaseAccessor
  def Flags_Covariant: Flags = core.Flags.Covariant
  def Flags_Contravariant: Flags = core.Flags.Contravariant
  def Flags_Scala2X: Flags = core.Flags.Scala2x
  def Flags_DefaultParameterized: Flags = core.Flags.DefaultParameterized
  def Flags_StableRealizable: Flags = core.Flags.StableRealizable
  def Flags_Param: Flags = core.Flags.Param
  def Flags_ParamAccessor: Flags = core.Flags.ParamAccessor
  def Flags_Enum: Flags = core.Flags.Enum
  def Flags_ModuleClass: Flags = core.Flags.ModuleClass
  def Flags_PrivateLocal: Flags = core.Flags.PrivateLocal
  def Flags_Package: Flags = core.Flags.Package

  //
  // QUOTED SEAL/UNSEAL
  //

  /** View this expression `quoted.Expr[?]` as a `Term` */
  def QuotedExpr_unseal(self: scala.quoted.Expr[?])(given Context): Term =
    PickledQuotes.quotedExprToTree(self)

  /** View this expression `quoted.Type[?]` as a `TypeTree` */
  def QuotedType_unseal(self: scala.quoted.Type[?])(given Context): TypeTree =
    PickledQuotes.quotedTypeToTree(self)

  /** Convert `Term` to an `quoted.Expr[Any]`  */
  def QuotedExpr_seal(self: Term)(given ctx: Context): scala.quoted.Expr[Any] = {
    def etaExpand(term: Term): Term = term.tpe.widen match {
      case mtpe: Types.MethodType if !mtpe.isParamDependent =>
        val closureResType = mtpe.resType match {
          case t: Types.MethodType => t.toFunctionType()
          case t => t
        }
        val closureTpe = Types.MethodType(mtpe.paramNames, mtpe.paramInfos, closureResType)
        val closureMethod = ctx.newSymbol(ctx.owner, nme.ANON_FUN, Synthetic | Method, closureTpe)
        tpd.Closure(closureMethod, tss => etaExpand(new tpd.TreeOps(term).appliedToArgs(tss.head)))
      case _ => term
    }
    new scala.internal.quoted.TastyTreeExpr(etaExpand(self), compilerId)
  }

  /** Checked cast to a `quoted.Expr[U]` */
  def QuotedExpr_cast[U](self: scala.quoted.Expr[?])(given tp: scala.quoted.Type[U], ctx: Context): scala.quoted.Expr[U] = {
    val tree = QuotedExpr_unseal(self)
    val expectedType = QuotedType_unseal(tp).tpe
    if (tree.tpe <:< expectedType)
      self.asInstanceOf[scala.quoted.Expr[U]]
    else
      throw new scala.tasty.reflect.ExprCastError(
        s"""Expr: ${tree.show}
           |did not conform to type: ${expectedType.show}
           |""".stripMargin
      )
  }

  /** Convert `Type` to an `quoted.Type[?]` */
  def QuotedType_seal(self: Type)(given ctx: Context): scala.quoted.Type[?] = {
    val dummySpan = ctx.owner.span // FIXME
    new scala.internal.quoted.TreeType(tpd.TypeTree(self).withSpan(dummySpan), compilerId)
  }

  //
  // DEFINITIONS
  //

  // Symbols

  def Definitions_RootPackage: Symbol = defn.RootPackage
  def Definitions_RootClass: Symbol = defn.RootClass

  def Definitions_EmptyPackageClass: Symbol = defn.EmptyPackageClass

  def Definitions_ScalaPackage: Symbol = defn.ScalaPackageVal
  def Definitions_ScalaPackageClass: Symbol = defn.ScalaPackageClass

  def Definitions_AnyClass: Symbol = defn.AnyClass
  def Definitions_AnyValClass: Symbol = defn.AnyValClass
  def Definitions_ObjectClass: Symbol = defn.ObjectClass
  def Definitions_AnyRefClass: Symbol = defn.AnyRefAlias
  def Definitions_NullClass: Symbol = defn.AnyClass
  def Definitions_NothingClass: Symbol = defn.NothingClass
  def Definitions_UnitClass: Symbol = defn.UnitClass
  def Definitions_ByteClass: Symbol = defn.ByteClass
  def Definitions_ShortClass: Symbol = defn.ShortClass
  def Definitions_CharClass: Symbol = defn.CharClass
  def Definitions_IntClass: Symbol = defn.IntClass
  def Definitions_LongClass: Symbol = defn.LongClass
  def Definitions_FloatClass: Symbol = defn.FloatClass
  def Definitions_DoubleClass: Symbol = defn.DoubleClass
  def Definitions_BooleanClass: Symbol = defn.BooleanClass
  def Definitions_StringClass: Symbol = defn.StringClass
  def Definitions_ClassClass: Symbol = defn.ClassClass
  def Definitions_ArrayClass: Symbol = defn.ArrayClass
  def Definitions_PredefModule: Symbol = defn.ScalaPredefModule.asTerm
  def Definitions_Predef_classOf: Symbol = defn.Predef_classOf.asTerm

  def Definitions_JavaLangPackage: Symbol = defn.JavaLangPackageVal

  def Definitions_ArrayModule: Symbol = defn.ArrayClass.companionModule.asTerm

  def Definitions_Array_apply: Symbol = defn.Array_apply.asTerm
  def Definitions_Array_clone: Symbol = defn.Array_clone.asTerm
  def Definitions_Array_length: Symbol = defn.Array_length.asTerm
  def Definitions_Array_update: Symbol = defn.Array_update.asTerm

  def Definitions_RepeatedParamClass: Symbol = defn.RepeatedParamClass

  def Definitions_OptionClass: Symbol = defn.OptionClass
  def Definitions_NoneModule: Symbol = defn.NoneModule
  def Definitions_SomeModule: Symbol = defn.SomeClass.companionModule.asTerm

  def Definitions_ProductClass: Symbol = defn.ProductClass
  def Definitions_FunctionClass(arity: Int, isImplicit: Boolean, isErased: Boolean): Symbol =
    defn.FunctionClass(arity, isImplicit, isErased).asClass
  def Definitions_TupleClass(arity: Int): Symbol = defn.TupleType(arity).classSymbol.asClass

  def Definitions_InternalQuoted_patternHole: Symbol = defn.InternalQuoted_patternHole
  def Definitions_InternalQuoted_patternBindHoleAnnot: Symbol = defn.InternalQuoted_patternBindHoleAnnot
  def Definitions_InternalQuoted_fromAboveAnnot: Symbol = defn.InternalQuoted_fromAboveAnnot

  // Types

  def Definitions_UnitType: Type = defn.UnitType
  def Definitions_ByteType: Type = defn.ByteType
  def Definitions_ShortType: Type = defn.ShortType
  def Definitions_CharType: Type = defn.CharType
  def Definitions_IntType: Type = defn.IntType
  def Definitions_LongType: Type = defn.LongType
  def Definitions_FloatType: Type = defn.FloatType
  def Definitions_DoubleType: Type = defn.DoubleType
  def Definitions_BooleanType: Type = defn.BooleanType
  def Definitions_AnyType: Type = defn.AnyType
  def Definitions_AnyValType: Type = defn.AnyValType
  def Definitions_AnyRefType: Type = defn.AnyRefType
  def Definitions_ObjectType: Type = defn.ObjectType
  def Definitions_NothingType: Type = defn.NothingType
  def Definitions_NullType: Type = defn.NullType
  def Definitions_StringType: Type = defn.StringType

  //
  // IMPLICITS
  //

  type ImplicitSearchResult = Tree

  def searchImplicit(tpe: Type)(given ctx: Context): ImplicitSearchResult =
    ctx.typer.inferImplicitArg(tpe, rootPosition.span)

  type ImplicitSearchSuccess = Tree
  def matchImplicitSearchSuccess(isr: ImplicitSearchResult)(given Context): Option[ImplicitSearchSuccess] = isr.tpe match {
    case _: SearchFailureType => None
    case _ => Some(isr)
  }
  def ImplicitSearchSuccess_tree(self: ImplicitSearchSuccess)(given Context): Term = self

  type ImplicitSearchFailure = Tree
  def matchImplicitSearchFailure(isr: ImplicitSearchResult)(given Context): Option[ImplicitSearchFailure] = isr.tpe match {
    case _: SearchFailureType => Some(isr)
    case _ => None
  }
  def ImplicitSearchFailure_explanation(self: ImplicitSearchFailure)(given Context): String =
    self.tpe.asInstanceOf[SearchFailureType].explanation

  type DivergingImplicit = Tree
  def matchDivergingImplicit(isr: ImplicitSearchResult)(given Context): Option[DivergingImplicit] = isr.tpe match {
    case _: Implicits.DivergingImplicit => Some(isr)
    case _ => None
  }

  type NoMatchingImplicits = Tree
  def matchNoMatchingImplicits(isr: ImplicitSearchResult)(given Context): Option[NoMatchingImplicits] = isr.tpe match {
    case _: Implicits.NoMatchingImplicits => Some(isr)
    case _ => None
  }

  type AmbiguousImplicits = Tree
  def matchAmbiguousImplicits(isr: ImplicitSearchResult)(given Context): Option[AmbiguousImplicits] = isr.tpe match {
    case _: Implicits.AmbiguousImplicits => Some(isr)
    case _ => None
  }

  def betaReduce(fn: Term, args: List[Term])(given ctx: Context): Term = {
    val (argVals0, argRefs0) = args.foldLeft((List.empty[ValDef], List.empty[Tree])) { case ((acc1, acc2), arg) => arg.tpe match {
      case tpe: SingletonType if isIdempotentExpr(arg) => (acc1, arg :: acc2)
      case _ =>
        val argVal = SyntheticValDef(NameKinds.UniqueName.fresh("x".toTermName), arg).withSpan(arg.span)
        (argVal :: acc1, ref(argVal.symbol) :: acc2)
    }}
    val argVals = argVals0.reverse
    val argRefs = argRefs0.reverse
    def rec(fn: Tree): Tree = fn match {
      case Inlined(call, bindings, expansion) =>
        // this case must go before closureDef to avoid dropping the inline node
        cpy.Inlined(fn)(call, bindings, rec(expansion))
      case closureDef(ddef) =>
        val paramSyms = ddef.vparamss.head.map(param => param.symbol)
        val paramToVals = paramSyms.zip(argRefs).toMap
        new TreeTypeMap(
          oldOwners = ddef.symbol :: Nil,
          newOwners = ctx.owner :: Nil,
          treeMap = tree => paramToVals.get(tree.symbol).map(_.withSpan(tree.span)).getOrElse(tree)
        ).transform(ddef.rhs)
      case Block(stats, expr) =>
        seq(stats, rec(expr)).withSpan(fn.span)
      case _ =>
        fn.select(nme.apply).appliedToArgs(argRefs).withSpan(fn.span)
    }
    seq(argVals, rec(fn))
  }

  //
  // HELPERS
  //

  private def optional[T <: Trees.Tree[?]](tree: T): Option[tree.type] =
    if (tree.isEmpty) None else Some(tree)

  private def withDefaultPos[T <: Tree](fn: (given Context) => T)(given ctx: Context): T =
    fn(given ctx.withSource(rootPosition.source)).withSpan(rootPosition.span)

  private def compilerId: Int = rootContext.outersIterator.toList.last.hashCode()
}

