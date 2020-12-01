package scala.quoted
package runtime.impl

import dotty.tools.dotc
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Annotations
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.quoted.reflect._
import dotty.tools.dotc.quoted.QuoteUtils._
import dotty.tools.dotc.core.Decorators._

import dotty.tools.dotc.quoted.{MacroExpansion, PickledQuotes, QuoteUtils}

import scala.quoted.runtime.{QuoteUnpickler, QuoteMatching}
import scala.quoted.runtime.impl.printers._

import scala.reflect.TypeTest

object QuotesImpl {

  type ScopeId = Int

  def apply()(using Context): Quotes =
    new QuotesImpl

  def showDecompiledTree(tree: tpd.Tree)(using Context): String =
    import qctx.reflect.TreeMethodsImpl.{showAnsiColored, show}
    val qctx: QuotesImpl = new QuotesImpl(using MacroExpansion.context(tree))
    if ctx.settings.color.value == "always" then showAnsiColored(tree)
    else show(tree)

  // TODO Explore more fine grained scope ids.
  //      This id can only differentiate scope extrusion from one compiler instance to another.
  def scopeId(using Context): ScopeId =
    ctx.outersIterator.toList.last.hashCode()

}

class QuotesImpl private (using val ctx: Context) extends Quotes, QuoteUnpickler, QuoteMatching:

  private val yCheck: Boolean =
    ctx.settings.Ycheck.value(using ctx).exists(x => x == "all" || x == "macros")

  extension [T](self: scala.quoted.Expr[T]):
    def show: String =
      reflect.TreeMethodsImpl.show(reflect.Term.of(self))

    def showAnsiColored: String =
      reflect.TreeMethodsImpl.showAnsiColored(reflect.Term.of(self))

    def matches(that: scala.quoted.Expr[Any]): Boolean =
      treeMatch(reflect.Term.of(self), reflect.Term.of(that)).nonEmpty

  end extension

  extension [X](self: scala.quoted.Expr[Any]):
    /** Checks is the `quoted.Expr[?]` is valid expression of type `X` */
    def isExprOf(using scala.quoted.Type[X]): Boolean =
      reflect.TypeReprMethodsImpl.<:<(reflect.Term.of(self).tpe)(reflect.TypeRepr.of[X])

    /** Convert this to an `quoted.Expr[X]` if this expression is a valid expression of type `X` or throws */
    def asExprOf(using scala.quoted.Type[X]): scala.quoted.Expr[X] = {
      if isExprOf[X] then
        self.asInstanceOf[scala.quoted.Expr[X]]
      else
        throw Exception(
          s"""Expr cast exception: ${self.show}
            |of type: ${reflect.TypeReprMethodsImpl.show(reflect.Term.of(self).tpe)}
            |did not conform to type: ${reflect.TypeReprMethodsImpl.show(reflect.TypeRepr.of[X])}
            |""".stripMargin
        )
    }
  end extension

  object reflect extends Reflection:

    type Tree = tpd.Tree

    object Tree extends TreeModule:
        def of(expr: Expr[Any]): Tree = Term.of(expr)
    end Tree

    object TreeMethodsImpl extends TreeMethods:
      extension (self: Tree):
        def pos: Position = self.sourcePos
        def symbol: Symbol = self.symbol
        def showExtractors: String =
          Extractors.showTree(using QuotesImpl.this)(self)
        def show: String =
          SourceCode.showTree(using QuotesImpl.this)(self)(SyntaxHighlight.plain, fullNames = true)
        def showShort: String =
          SourceCode.showTree(using QuotesImpl.this)(self)(SyntaxHighlight.plain, fullNames = false)
        def showAnsiColored: String =
          SourceCode.showTree(using QuotesImpl.this)(self)(SyntaxHighlight.ANSI, fullNames = true)
        def isExpr: Boolean =
          self match
            case TermTypeTest(self) =>
              self.tpe.widen match
                case _: MethodType | _: PolyType => false
                case _ => true
            case _ => false
        def asExpr: scala.quoted.Expr[Any] =
          if self.isExpr then
            new ExprImpl(self, QuotesImpl.this.hashCode)
          else self match
            case TermTypeTest(self) => throw new Exception("Expected an expression. This is a partially applied Term. Try eta-expanding the term first.")
            case _ => throw new Exception("Expected a Term but was: " + self)
      end extension

      extension [T](self: Tree)
        def asExprOf(using tp: scala.quoted.Type[T]): scala.quoted.Expr[T] =
          QuotesImpl.this.asExprOf[T](self.asExpr)(using tp)
      end extension

      extension [ThisTree <: Tree](self: ThisTree):
        def changeOwner(newOwner: Symbol): ThisTree =
          tpd.TreeOps(self).changeNonLocalOwners(newOwner).asInstanceOf[ThisTree]
      end extension

    end TreeMethodsImpl

    type PackageClause = tpd.PackageDef

    object PackageClauseTypeTestImpl extends TypeTest[Tree, PackageClause]:
      def unapply(x: Tree): Option[PackageClause & x.type] = x match
        case x: (tpd.PackageDef & x.type) => Some(x)
        case _ => None
    end PackageClauseTypeTestImpl

    object PackageClause extends PackageClauseModule:
      def apply(pid: Ref, stats: List[Tree]): PackageClause =
        withDefaultPos(tpd.PackageDef(pid.asInstanceOf[tpd.RefTree], stats))
      def copy(original: Tree)(pid: Ref, stats: List[Tree]): PackageClause =
        tpd.cpy.PackageDef(original)(pid, stats)
      def unapply(tree: PackageClause): Some[(Ref, List[Tree])] =
        Some((tree.pid, tree.stats))
    end PackageClause

    object PackageClauseMethodsImpl extends PackageClauseMethods:
      extension (self: PackageClause):
        def pid: Ref = self.pid
        def stats: List[Tree] = self.stats
      end extension
    end PackageClauseMethodsImpl

    type Import = tpd.Import

    object ImportTypeTestImpl extends TypeTest[Tree, Import]:
      def unapply(x: Tree): Option[Import & x.type] = x match
        case tree: (tpd.Import & x.type) => Some(tree)
        case _ => None
    end ImportTypeTestImpl

    object Import extends ImportModule:
      def apply(expr: Term, selectors: List[ImportSelector]): Import =
        withDefaultPos(tpd.Import(expr, selectors))
      def copy(original: Tree)(expr: Term, selectors: List[ImportSelector]): Import =
        tpd.cpy.Import(original)(expr, selectors)
      def unapply(tree: Import): Option[(Term, List[ImportSelector])] =
        Some((tree.expr, tree.selectors))
    end Import

    object ImportMethodsImpl extends ImportMethods:
      extension (self: Import):
        def expr: Term = self.expr
        def selectors: List[ImportSelector] = self.selectors
      end extension
    end ImportMethodsImpl

    type Statement = tpd.Tree

    object StatementTypeTestImpl extends TypeTest[Tree, Statement]:
      def unapply(x: Tree): Option[Statement & x.type] = x match
        case _: tpd.PatternTree => None
        case _ =>
          if x.isTerm then TermTypeTest.unapply(x)
          else DefinitionTypeTest.unapply(x)
    end StatementTypeTestImpl

    type Definition = tpd.MemberDef

    object DefinitionTypeTestImpl extends TypeTest[Tree, Definition]:
      def unapply(x: Tree): Option[Definition & x.type] = x match
        case x: (tpd.MemberDef & x.type) => Some(x)
        case _ => None
    end DefinitionTypeTestImpl

    object Definition extends DefinitionModule

    object DefinitionMethodsImpl extends DefinitionMethods:
      extension (self: Definition):
        def name: String = self match
          case self: tpd.MemberDef => self.name.toString
      end extension
    end DefinitionMethodsImpl

    type ClassDef = tpd.TypeDef

    object ClassDefTypeTestImpl extends TypeTest[Tree, ClassDef]:
      def unapply(x: Tree): Option[ClassDef & x.type] = x match
        case x: (tpd.TypeDef & x.type) if x.isClassDef => Some(x)
        case _ => None
    end ClassDefTypeTestImpl

    object ClassDef extends ClassDefModule:
      def copy(original: Tree)(name: String, constr: DefDef, parents: List[Tree], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement]): ClassDef = {
        val dotc.ast.Trees.TypeDef(_, originalImpl: tpd.Template) = original
        tpd.cpy.TypeDef(original)(name.toTypeName, tpd.cpy.Template(originalImpl)(constr, parents, derived, selfOpt.getOrElse(tpd.EmptyValDef), body))
      }
      def unapply(cdef: ClassDef): Option[(String, DefDef, List[Tree /* Term | TypeTree */], List[TypeTree], Option[ValDef], List[Statement])] =
        val rhs = cdef.rhs.asInstanceOf[tpd.Template]
        Some((cdef.name.toString, cdef.constructor, cdef.parents, rhs.derived.asInstanceOf[List[TypeTree]], cdef.self, rhs.body))
    end ClassDef

    object ClassDefMethodsImpl extends ClassDefMethods:
      extension (self: ClassDef):
        def constructor: DefDef =
          self.rhs.asInstanceOf[tpd.Template].constr
        def parents: List[Tree] =
          self.rhs.asInstanceOf[tpd.Template].parents
        def derived: List[TypeTree] =
          self.rhs.asInstanceOf[tpd.Template].derived.asInstanceOf[List[TypeTree]]
        def self: Option[ValDef] =
          optional(self.rhs.asInstanceOf[tpd.Template].self)
        def body: List[Statement] =
          self.rhs.asInstanceOf[tpd.Template].body
      end extension
    end ClassDefMethodsImpl

    type DefDef = tpd.DefDef

    object DefDefTypeTestImpl extends TypeTest[Tree, DefDef]:
      def unapply(x: Tree): Option[DefDef & x.type] = x match
        case x: (tpd.DefDef & x.type) => Some(x)
        case _ => None
    end DefDefTypeTestImpl

    object DefDef extends DefDefModule:
      def apply(symbol: Symbol, rhsFn: List[TypeRepr] => List[List[Term]] => Option[Term]): DefDef =
        withDefaultPos(tpd.polyDefDef(symbol.asTerm, tparams => vparamss => yCheckedOwners(rhsFn(tparams)(vparamss), symbol).getOrElse(tpd.EmptyTree)))
      def copy(original: Tree)(name: String, typeParams: List[TypeDef], paramss: List[List[ValDef]], tpt: TypeTree, rhs: Option[Term]): DefDef =
        tpd.cpy.DefDef(original)(name.toTermName, typeParams, paramss, tpt, yCheckedOwners(rhs, original.symbol).getOrElse(tpd.EmptyTree))
      def unapply(ddef: DefDef): Option[(String, List[TypeDef], List[List[ValDef]], TypeTree, Option[Term])] =
        Some((ddef.name.toString, ddef.typeParams, ddef.paramss, ddef.tpt, optional(ddef.rhs)))
    end DefDef

    object DefDefMethodsImpl extends DefDefMethods:
      extension (self: DefDef):
        def typeParams: List[TypeDef] = self.tparams
        def paramss: List[List[ValDef]] = self.vparamss
        def returnTpt: TypeTree = self.tpt
        def rhs: Option[Term] = optional(self.rhs)
      end extension
    end DefDefMethodsImpl

    type ValDef = tpd.ValDef

    object ValDefTypeTestImpl extends TypeTest[Tree, ValDef]:
      def unapply(x: Tree): Option[ValDef & x.type] = x match
        case x: (tpd.ValDef & x.type) => Some(x)
        case _ => None
    end ValDefTypeTestImpl

    object ValDef extends ValDefModule:
      def apply(symbol: Symbol, rhs: Option[Term]): ValDef =
        tpd.ValDef(symbol.asTerm, yCheckedOwners(rhs, symbol).getOrElse(tpd.EmptyTree))
      def copy(original: Tree)(name: String, tpt: TypeTree, rhs: Option[Term]): ValDef =
        tpd.cpy.ValDef(original)(name.toTermName, tpt, yCheckedOwners(rhs, original.symbol).getOrElse(tpd.EmptyTree))
      def unapply(vdef: ValDef): Option[(String, TypeTree, Option[Term])] =
        Some((vdef.name.toString, vdef.tpt, optional(vdef.rhs)))

      def let(owner: Symbol, name: String, rhs: Term)(body: Ident => Term): Term =
        val vdef = tpd.SyntheticValDef(name.toTermName, rhs)(using ctx.withOwner(owner))
        val ref = tpd.ref(vdef.symbol).asInstanceOf[Ident]
        Block(List(vdef), body(ref))

      def let(owner: Symbol, terms: List[Term])(body: List[Ident] => Term): Term =
        val ctx1 = ctx.withOwner(owner)
        val vdefs = terms.map(term => tpd.SyntheticValDef("x".toTermName, term)(using ctx1))
        val refs = vdefs.map(vdef => tpd.ref(vdef.symbol).asInstanceOf[Ident])
        Block(vdefs, body(refs))
    end ValDef

    object ValDefMethodsImpl extends ValDefMethods:
      extension (self: ValDef):
        def tpt: TypeTree = self.tpt
        def rhs: Option[Term] = optional(self.rhs)
      end extension
    end ValDefMethodsImpl

    type TypeDef = tpd.TypeDef

    object TypeDefTypeTestImpl extends TypeTest[Tree, TypeDef]:
      def unapply(x: Tree): Option[TypeDef & x.type] = x match
        case x: (tpd.TypeDef & x.type) if !x.isClassDef => Some(x)
        case _ => None
    end TypeDefTypeTestImpl

    object TypeDef extends TypeDefModule:
      def apply(symbol: Symbol): TypeDef =
        withDefaultPos(tpd.TypeDef(symbol.asType))
      def copy(original: Tree)(name: String, rhs: Tree): TypeDef =
        tpd.cpy.TypeDef(original)(name.toTypeName, rhs)
      def unapply(tdef: TypeDef): Option[(String, Tree /*TypeTree | TypeBoundsTree*/ /* TypeTree | TypeBoundsTree */)] =
        Some((tdef.name.toString, tdef.rhs))
    end TypeDef

    object TypeDefMethodsImpl extends TypeDefMethods:
      extension (self: TypeDef):
        def rhs: Tree = self.rhs
      end extension
    end TypeDefMethodsImpl

    type Term = tpd.Tree

    object TermTypeTestImpl extends TypeTest[Tree, Term]:
      def unapply(x: Tree): Option[Term & x.type] = x match
        case _ if UnapplyTypeTest.unapply(x).isDefined => None
        case _: tpd.PatternTree => None
        case x: (tpd.Tree & x.type) if x.isTerm => Some(x)
        case x: (tpd.SeqLiteral & x.type) => Some(x)
        case x: (tpd.Inlined & x.type) => Some(x)
        case x: (tpd.NamedArg & x.type) => Some(x)
        case _ => None
    end TermTypeTestImpl

    object Term extends TermModule:
      def of(expr: Expr[Any]): Term =
        val exprImpl = expr.asInstanceOf[ExprImpl]
        exprImpl.checkScopeId(QuotesImpl.this.hashCode)
        exprImpl.tree

      def betaReduce(tree: Term): Option[Term] =
        tree match
          case app @ tpd.Apply(tpd.Select(fn, nme.apply), args) if dotc.core.Symbols.defn.isFunctionType(fn.tpe) =>
            val app1 = dotc.transform.BetaReduce(app, fn, args)
            if app1 eq app then None
            else Some(app1.withSpan(tree.span))
          case tpd.Block(Nil, expr) =>
            for e <- betaReduce(expr) yield tpd.cpy.Block(tree)(Nil, e)
          case tpd.Inlined(_, Nil, expr) =>
            betaReduce(expr)
          case _ =>
            None
    end Term

    object TermMethodsImpl extends TermMethods:
      extension (self: Term):
        def seal: scala.quoted.Expr[Any] =
          if self.isExpr then new ExprImpl(self, QuotesImpl.this.hashCode)
          else throw new Exception("Cannot seal a partially applied Term. Try eta-expanding the term first.")

        def sealOpt: Option[scala.quoted.Expr[Any]] =
          if self.isExpr then Some(new ExprImpl(self, QuotesImpl.this.hashCode))
          else None

        def tpe: TypeRepr = self.tpe
        def underlyingArgument: Term = new tpd.TreeOps(self).underlyingArgument
        def underlying: Term = new tpd.TreeOps(self).underlying
        def etaExpand(owner: Symbol): Term = self.tpe.widen match {
          case mtpe: Types.MethodType if !mtpe.isParamDependent =>
            val closureResType = mtpe.resType match {
              case t: Types.MethodType => t.toFunctionType()
              case t => t
            }
            val closureTpe = Types.MethodType(mtpe.paramNames, mtpe.paramInfos, closureResType)
            val closureMethod = dotc.core.Symbols.newSymbol(owner, nme.ANON_FUN, Synthetic | Method, closureTpe)
            tpd.Closure(closureMethod, tss => new tpd.TreeOps(self).appliedToArgs(tss.head).etaExpand(closureMethod))
          case _ => self
        }

        def appliedTo(arg: Term): Term =
          self.appliedToArgs(arg :: Nil)
        def appliedTo(arg: Term, args: Term*): Term =
          self.appliedToArgs(arg :: args.toList)
        def appliedToArgs(args: List[Term]): Apply =
          Apply(self, args)
        def appliedToArgss(argss: List[List[Term]]): Term =
          argss.foldLeft(self: Term)(Apply(_, _))
        def appliedToNone: Apply =
          self.appliedToArgs(Nil)
        def appliedToType(targ: TypeRepr): Term =
          self.appliedToTypes(targ :: Nil)
        def appliedToTypes(targs: List[TypeRepr]): Term =
          self.appliedToTypeTrees(targs map (Inferred(_)))
        def appliedToTypeTrees(targs: List[TypeTree]): Term =
          if (targs.isEmpty) self else TypeApply(self, targs)
        def select(sym: Symbol): Select = Select(self, sym)

      end extension
    end TermMethodsImpl

    type Ref = tpd.RefTree

    object RefTypeTestImpl extends TypeTest[Tree, Ref]:
      def unapply(x: Tree): Option[Ref & x.type] = x match
        case x: (tpd.RefTree & x.type) if x.isTerm => Some(x)
        case _ => None
    end RefTypeTestImpl

    object Ref extends RefModule:
      def term(tp: TermRef): Ref =
        withDefaultPos(tpd.ref(tp).asInstanceOf[tpd.RefTree])
      def apply(sym: Symbol): Ref =
        assert(sym.isTerm)
        withDefaultPos(tpd.ref(sym).asInstanceOf[tpd.RefTree])
    end Ref

    type Ident = tpd.Ident

    object IdentTypeTestImpl extends TypeTest[Tree, Ident]:
      def unapply(x: Tree): Option[Ident & x.type] = x match
        case x: (tpd.Ident & x.type) if x.isTerm => Some(x)
        case _ => None
    end IdentTypeTestImpl

    object Ident extends IdentModule:
      def apply(tmref: TermRef): Term =
        withDefaultPos(tpd.ref(tmref).asInstanceOf[Term])
      def copy(original: Tree)(name: String): Ident =
        tpd.cpy.Ident(original)(name.toTermName)
      def unapply(tree: Ident): Option[String] =
        Some(tree.name.toString)
    end Ident

    object IdentMethodsImpl extends IdentMethods:
      extension (self: Ident):
        def name: String = self.name.toString
      end extension
    end IdentMethodsImpl

    type Select = tpd.Select

    object SelectTypeTestImpl extends TypeTest[Tree, Select]:
      def unapply(x: Tree): Option[Select & x.type] = x match
        case x: (tpd.Select & x.type) if x.isTerm => Some(x)
        case _ => None
    end SelectTypeTestImpl

    object Select extends SelectModule:
      def apply(qualifier: Term, symbol: Symbol): Select =
        withDefaultPos(tpd.Select(qualifier, Types.TermRef(qualifier.tpe, symbol)))
      def unique(qualifier: Term, name: String): Select =
        val denot = qualifier.tpe.member(name.toTermName)
        assert(!denot.isOverloaded, s"The symbol `$name` is overloaded. The method Select.unique can only be used for non-overloaded symbols.")
        withDefaultPos(tpd.Select(qualifier, name.toTermName))
      def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term]): Apply =
        withDefaultPos(tpd.applyOverloaded(qualifier, name.toTermName, args, targs, Types.WildcardType).asInstanceOf[Apply])

      def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term], returnType: TypeRepr): Apply =
        withDefaultPos(tpd.applyOverloaded(qualifier, name.toTermName, args, targs, returnType).asInstanceOf[Apply])
      def copy(original: Tree)(qualifier: Term, name: String): Select =
        tpd.cpy.Select(original)(qualifier, name.toTermName)
      def unapply(x: Select): Option[(Term, String)] =
        Some((x.qualifier, x.name.toString))
    end Select

    object SelectMethodsImpl extends SelectMethods:
      extension (self: Select):
        def qualifier: Term = self.qualifier
        def name: String = self.name.toString
        def signature: Option[Signature] =
          if self.symbol.signature == dotc.core.Signature.NotAMethod then None
          else Some(self.symbol.signature)
      end extension
    end SelectMethodsImpl

    type Literal = tpd.Literal

    object LiteralTypeTestImpl extends TypeTest[Tree, Literal]:
      def unapply(x: Tree): Option[Literal & x.type] = x match
        case x: (tpd.Literal & x.type) => Some(x)
        case _ => None
    end LiteralTypeTestImpl

    object Literal extends LiteralModule:
      def apply(constant: Constant): Literal =
        withDefaultPos(tpd.Literal(constant))
      def copy(original: Tree)(constant: Constant): Literal =
        tpd.cpy.Literal(original)(constant)
      def unapply(x: Literal): Option[Constant] =
        Some(x.constant)
    end Literal

    object LiteralMethodsImpl extends LiteralMethods:
      extension (self: Literal):
        def constant: Constant = self.const
      end extension
    end LiteralMethodsImpl

    type This = tpd.This

    object ThisTypeTestImpl extends TypeTest[Tree, This]:
      def unapply(x: Tree): Option[This & x.type] = x match
        case x: (tpd.This & x.type) => Some(x)
        case _ => None
    end ThisTypeTestImpl

    object This extends ThisModule:
      def apply(cls: Symbol): This =
        withDefaultPos(tpd.This(cls.asClass))
      def copy(original: Tree)(qual: Option[String]): This =
        tpd.cpy.This(original)(qual.map(x => untpd.Ident(x.toTypeName)).getOrElse(untpd.EmptyTypeIdent))
      def unapply(x: This): Option[Option[String]] =
        Some(optional(x.qual).map(_.name.toString))
    end This

    object ThisMethodsImpl extends ThisMethods:
      extension (self: This):
        def id: Option[String] = optional(self.qual).map(_.name.toString)
      end extension
    end ThisMethodsImpl

    type New = tpd.New

    object NewTypeTestImpl extends TypeTest[Tree, New]:
      def unapply(x: Tree): Option[New & x.type] = x match
        case x: (tpd.New & x.type) => Some(x)
        case _ => None
    end NewTypeTestImpl

    object New extends NewModule:
      def apply(tpt: TypeTree): New =
        withDefaultPos(tpd.New(tpt))
      def copy(original: Tree)(tpt: TypeTree): New =
        tpd.cpy.New(original)(tpt)
      def unapply(x: New): Option[TypeTree] = Some(x.tpt)
    end New

    object NewMethodsImpl extends NewMethods:
      extension (self: New):
        def tpt: TypeTree = self.tpt
      end extension
    end NewMethodsImpl

    type NamedArg = tpd.NamedArg

    object NamedArgTypeTestImpl extends TypeTest[Tree, NamedArg]:
      def unapply(x: Tree): Option[NamedArg & x.type] = x match
        case x: (tpd.NamedArg & x.type) if x.name.isInstanceOf[dotc.core.Names.TermName] => Some(x) // TODO: Now, the name should alwas be a term name
        case _ => None
    end NamedArgTypeTestImpl

    object NamedArg extends NamedArgModule:
      def apply(name: String, arg: Term): NamedArg =
        withDefaultPos(tpd.NamedArg(name.toTermName, arg))
      def copy(original: Tree)(name: String, arg: Term): NamedArg =
        tpd.cpy.NamedArg(original)(name.toTermName, arg)
      def unapply(x: NamedArg): Option[(String, Term)] =
        Some((x.name.toString, x.value))
    end NamedArg

    object NamedArgMethodsImpl extends NamedArgMethods:
      extension (self: NamedArg):
        def name: String = self.name.toString
        def value: Term = self.arg
      end extension
    end NamedArgMethodsImpl

    type Apply = tpd.Apply

    object ApplyTypeTestImpl extends TypeTest[Tree, Apply]:
      def unapply(x: Tree): Option[Apply & x.type] = x match
        case x: (tpd.Apply & x.type) => Some(x)
        case _ => None
    end ApplyTypeTestImpl

    object Apply extends ApplyModule:
      def apply(fun: Term, args: List[Term]): Apply =
        withDefaultPos(tpd.Apply(fun, args))
      def copy(original: Tree)(fun: Term, args: List[Term]): Apply =
        tpd.cpy.Apply(original)(fun, args)
      def unapply(x: Apply): Option[(Term, List[Term])] =
        Some((x.fun, x.args))
    end Apply

    object ApplyMethodsImpl extends ApplyMethods:
      extension (self: Apply):
        def fun: Term = self.fun
        def args: List[Term] = self.args
      end extension
    end ApplyMethodsImpl

    type TypeApply = tpd.TypeApply

    object TypeApplyTypeTestImpl extends TypeTest[Tree, TypeApply]:
      def unapply(x: Tree): Option[TypeApply & x.type] = x match
        case x: (tpd.TypeApply & x.type) => Some(x)
        case _ => None
    end TypeApplyTypeTestImpl

    object TypeApply extends TypeApplyModule:
      def apply(fun: Term, args: List[TypeTree]): TypeApply =
        withDefaultPos(tpd.TypeApply(fun, args))
      def copy(original: Tree)(fun: Term, args: List[TypeTree]): TypeApply =
        tpd.cpy.TypeApply(original)(fun, args)
      def unapply(x: TypeApply): Option[(Term, List[TypeTree])] =
        Some((x.fun, x.args))
    end TypeApply

    object TypeApplyMethodsImpl extends TypeApplyMethods:
      extension (self: TypeApply):
        def fun: Term = self.fun
        def args: List[TypeTree] = self.args
      end extension
    end TypeApplyMethodsImpl

    type Super = tpd.Super

    object SuperTypeTestImpl extends TypeTest[Tree, Super]:
      def unapply(x: Tree): Option[Super & x.type] = x match
        case x: (tpd.Super & x.type) => Some(x)
        case _ => None
    end SuperTypeTestImpl

    object Super extends SuperModule:
      def apply(qual: Term, mix: Option[String]): Super =
        withDefaultPos(tpd.Super(qual, mix.map(x => untpd.Ident(x.toTypeName)).getOrElse(untpd.EmptyTypeIdent), dotc.core.Symbols.NoSymbol))
      def copy(original: Tree)(qual: Term, mix: Option[String]): Super =
        tpd.cpy.Super(original)(qual, mix.map(x => untpd.Ident(x.toTypeName)).getOrElse(untpd.EmptyTypeIdent))
      def unapply(x: Super): Option[(Term, Option[String])] =
        Some((x.qualifier, x.id))
    end Super

    object SuperMethodsImpl extends SuperMethods:
      extension (self: Super):
        def qualifier: Term = self.qual
        def id: Option[String] = optional(self.mix).map(_.name.toString)
        def idPos: Position = self.mix.sourcePos
      end extension
    end SuperMethodsImpl

    type Typed = tpd.Typed

    object TypedTypeTestImpl extends TypeTest[Tree, Typed]:
      def unapply(x: Tree): Option[Typed & x.type] = x match
        case x: (tpd.Typed & x.type) => Some(x)
        case _ => None
    end TypedTypeTestImpl

    object Typed extends TypedModule:
      def apply(expr: Term, tpt: TypeTree): Typed =
        withDefaultPos(tpd.Typed(expr, tpt))
      def copy(original: Tree)(expr: Term, tpt: TypeTree): Typed =
        tpd.cpy.Typed(original)(expr, tpt)
      def unapply(x: Typed): Option[(Term, TypeTree)] =
        Some((x.expr, x.tpt))
    end Typed

    object TypedMethodsImpl extends TypedMethods:
      extension (self: Typed):
        def expr: Term = self.expr
        def tpt: TypeTree = self.tpt
      end extension
    end TypedMethodsImpl

    type Assign = tpd.Assign

    object AssignTypeTestImpl extends TypeTest[Tree, Assign]:
      def unapply(x: Tree): Option[Assign & x.type] = x match
        case x: (tpd.Assign & x.type) => Some(x)
        case _ => None
    end AssignTypeTestImpl

    object Assign extends AssignModule:
      def apply(lhs: Term, rhs: Term): Assign =
        withDefaultPos(tpd.Assign(lhs, rhs))
      def copy(original: Tree)(lhs: Term, rhs: Term): Assign =
        tpd.cpy.Assign(original)(lhs, rhs)
      def unapply(x: Assign): Option[(Term, Term)] =
        Some((x.lhs, x.rhs))
    end Assign

    object AssignMethodsImpl extends AssignMethods:
      extension (self: Assign):
        def lhs: Term = self.lhs
        def rhs: Term = self.rhs
      end extension
    end AssignMethodsImpl

    type Block = tpd.Block

    object BlockTypeTestImpl extends TypeTest[Tree, Block]:
      def unapply(x: Tree): Option[Block & x.type] = x match
        case x: (tpd.Block & x.type) => Some(x)
        case _ => None
    end BlockTypeTestImpl

    object Block extends BlockModule:
      def apply(stats: List[Statement], expr: Term): Block =
        withDefaultPos(tpd.Block(stats, expr))
      def copy(original: Tree)(stats: List[Statement], expr: Term): Block =
        tpd.cpy.Block(original)(stats, expr)
      def unapply(x: Block): Option[(List[Statement], Term)] =
        Some((x.statements, x.expr))
    end Block

    object BlockMethodsImpl extends BlockMethods:
      extension (self: Block):
        def statements: List[Statement] = self.stats
        def expr: Term = self.expr
      end extension
    end BlockMethodsImpl

    type Closure = tpd.Closure

    object ClosureTypeTestImpl extends TypeTest[Tree, Closure]:
      def unapply(x: Tree): Option[Closure & x.type] = x match
        case x: (tpd.Closure & x.type) => Some(x)
        case _ => None
    end ClosureTypeTestImpl

    object Closure extends ClosureModule:
      def apply(meth: Term, tpe: Option[TypeRepr]): Closure =
        withDefaultPos(tpd.Closure(Nil, meth, tpe.map(tpd.TypeTree(_)).getOrElse(tpd.EmptyTree)))
      def copy(original: Tree)(meth: Tree, tpe: Option[TypeRepr]): Closure =
        tpd.cpy.Closure(original)(Nil, meth, tpe.map(tpd.TypeTree(_)).getOrElse(tpd.EmptyTree))
      def unapply(x: Closure): Option[(Term, Option[TypeRepr])] =
        Some((x.meth, x.tpeOpt))
    end Closure

    object ClosureMethodsImpl extends ClosureMethods:
      extension (self: Closure):
        def meth: Term = self.meth
        def tpeOpt: Option[TypeRepr] = optional(self.tpt).map(_.tpe)
      end extension
    end ClosureMethodsImpl

    object Lambda extends LambdaModule:
      def apply(owner: Symbol, tpe: MethodType, rhsFn: (Symbol, List[Tree]) => Tree): Block =
        val meth = dotc.core.Symbols.newSymbol(owner, nme.ANON_FUN, Synthetic | Method, tpe)
        tpd.Closure(meth, tss => yCheckedOwners(rhsFn(meth, tss.head), meth))

      def unapply(tree: Block): Option[(List[ValDef], Term)] = tree match {
        case Block((ddef @ DefDef(_, _, params :: Nil, _, Some(body))) :: Nil, Closure(meth, _))
        if ddef.symbol == meth.symbol =>
          Some((params, body))
        case _ => None
      }
    end Lambda

    type If = tpd.If

    object IfTypeTestImpl extends TypeTest[Tree, If]:
      def unapply(x: Tree): Option[If & x.type] = x match
        case x: (tpd.If & x.type) => Some(x)
        case _ => None
    end IfTypeTestImpl

    object If extends IfModule:
      def apply(cond: Term, thenp: Term, elsep: Term): If =
        withDefaultPos(tpd.If(cond, thenp, elsep))
      def copy(original: Tree)(cond: Term, thenp: Term, elsep: Term): If =
        tpd.cpy.If(original)(cond, thenp, elsep)
      def unapply(tree: If): Option[(Term, Term, Term)] =
        Some((tree.cond, tree.thenp, tree.elsep))
    end If

    object IfMethodsImpl extends IfMethods:
      extension (self: If):
        def cond: Term = self.cond
        def thenp: Term = self.thenp
        def elsep: Term = self.elsep
      end extension
    end IfMethodsImpl

    type Match = tpd.Match

    object MatchTypeTestImpl extends TypeTest[Tree, Match]:
      def unapply(x: Tree): Option[Match & x.type] = x match
        case x: (tpd.Match & x.type) if !x.selector.isEmpty => Some(x)
        case _ => None
    end MatchTypeTestImpl

    object Match extends MatchModule:
      def apply(selector: Term, cases: List[CaseDef]): Match =
        withDefaultPos(tpd.Match(selector, cases))

      def copy(original: Tree)(selector: Term, cases: List[CaseDef]): Match =
        tpd.cpy.Match(original)(selector, cases)

      def unapply(x: Match): Option[(Term, List[CaseDef])] =
        Some((x.scrutinee, x.cases))
    end Match

    object MatchMethodsImpl extends MatchMethods:
      extension (self: Match):
        def scrutinee: Term = self.selector
        def cases: List[CaseDef] = self.cases
      end extension
    end MatchMethodsImpl

    type SummonFrom = tpd.Match

    object SummonFromTypeTestImpl extends TypeTest[Tree, SummonFrom]:
      def unapply(x: Tree): Option[SummonFrom & x.type] = x match
        case x: (tpd.Match & x.type) if x.selector.isEmpty => Some(x)
        case _ => None
    end SummonFromTypeTestImpl

    object SummonFrom extends SummonFromModule:
      def apply(cases: List[CaseDef]): SummonFrom =
        withDefaultPos(tpd.Match(tpd.EmptyTree, cases))
      def copy(original: Tree)(cases: List[CaseDef]): SummonFrom =
        tpd.cpy.Match(original)(tpd.EmptyTree, cases)
      def unapply(x: SummonFrom): Option[List[CaseDef]] =
        Some(x.cases)
    end SummonFrom

    object SummonFromMethodsImpl extends SummonFromMethods:
      extension (self: SummonFrom):
        def cases: List[CaseDef] = self.cases
      end extension
    end SummonFromMethodsImpl

    type Try = tpd.Try

    object TryTypeTestImpl extends TypeTest[Tree, Try]:
      def unapply(x: Tree): Option[Try & x.type] = x match
        case x: (tpd.Try & x.type) => Some(x)
        case _ => None
    end TryTypeTestImpl

    object Try extends TryModule:
      def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try =
        withDefaultPos(tpd.Try(expr, cases, finalizer.getOrElse(tpd.EmptyTree)))
      def copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try =
        tpd.cpy.Try(original)(expr, cases, finalizer.getOrElse(tpd.EmptyTree))
      def unapply(x: Try): Option[(Term, List[CaseDef], Option[Term])] =
        Some((x.body, x.cases, optional(x.finalizer)))
    end Try

    object TryMethodsImpl extends TryMethods:
      extension (self: Try):
        def body: Term = self.expr
        def cases: List[CaseDef] = self.cases
        def finalizer: Option[Term] = optional(self.finalizer)
      end extension
    end TryMethodsImpl

    type Return = tpd.Return

    object ReturnTypeTestImpl extends TypeTest[Tree, Return]:
      def unapply(x: Tree): Option[Return & x.type] = x match
        case x: (tpd.Return & x.type) => Some(x)
        case _ => None
    end ReturnTypeTestImpl

    object Return extends ReturnModule:
      def apply(expr: Term, from: Symbol): Return =
        withDefaultPos(tpd.Return(expr, from))
      def copy(original: Tree)(expr: Term, from: Symbol): Return =
        tpd.cpy.Return(original)(expr, tpd.ref(from))
      def unapply(x: Return): Option[(Term, Symbol)] =
        Some((x.expr, x.from.symbol))
    end Return

    object ReturnMethodsImpl extends ReturnMethods:
      extension (self: Return):
        def expr: Term = self.expr
        def from: Symbol = self.from.symbol
      end extension
    end ReturnMethodsImpl

    type Repeated = tpd.SeqLiteral

    object RepeatedTypeTestImpl extends TypeTest[Tree, Repeated]:
      def unapply(x: Tree): Option[Repeated & x.type] = x match
        case x: (tpd.SeqLiteral & x.type) => Some(x)
        case _ => None
    end RepeatedTypeTestImpl

    object Repeated extends RepeatedModule:
      def apply(elems: List[Term], elemtpt: TypeTree): Repeated =
        withDefaultPos(tpd.SeqLiteral(elems, elemtpt))
      def copy(original: Tree)(elems: List[Term], elemtpt: TypeTree): Repeated =
        tpd.cpy.SeqLiteral(original)(elems, elemtpt)
      def unapply(x: Repeated): Option[(List[Term], TypeTree)] =
        Some((x.elems, x.elemtpt))
    end Repeated

    object RepeatedMethodsImpl extends RepeatedMethods:
      extension (self: Repeated):
        def elems: List[Term] = self.elems
        def elemtpt: TypeTree = self.elemtpt
      end extension
    end RepeatedMethodsImpl

    type Inlined = tpd.Inlined

    object InlinedTypeTestImpl extends TypeTest[Tree, Inlined]:
      def unapply(x: Tree): Option[Inlined & x.type] = x match
        case x: (tpd.Inlined & x.type) => Some(x)
        case _ => None
    end InlinedTypeTestImpl

    object Inlined extends InlinedModule:
      def apply(call: Option[Tree], bindings: List[Definition], expansion: Term): Inlined =
        withDefaultPos(tpd.Inlined(call.getOrElse(tpd.EmptyTree), bindings.map { case b: tpd.MemberDef => b }, expansion))
      def copy(original: Tree)(call: Option[Tree], bindings: List[Definition], expansion: Term): Inlined =
        tpd.cpy.Inlined(original)(call.getOrElse(tpd.EmptyTree), bindings.asInstanceOf[List[tpd.MemberDef]], expansion)
      def unapply(x: Inlined): Option[(Option[Tree /* Term | TypeTree */], List[Definition], Term)] =
        Some((optional(x.call), x.bindings, x.body))
    end Inlined

    object InlinedMethodsImpl extends InlinedMethods:
      extension (self: Inlined):
        def call: Option[Tree] = optional(self.call)
        def bindings: List[Definition] = self.bindings
        def body: Term = self.expansion
      end extension
    end InlinedMethodsImpl

    type SelectOuter = tpd.Select

    object SelectOuterTypeTestImpl extends TypeTest[Tree, SelectOuter]:
      def unapply(x: Tree): Option[SelectOuter & x.type] = x match
        case x: (tpd.Select & x.type) =>
          x.name match
            case NameKinds.OuterSelectName(_, _) => Some(x)
            case _ => None
        case _ => None
    end SelectOuterTypeTestImpl

    object SelectOuter extends SelectOuterModule:
      def apply(qualifier: Term, name: String, levels: Int): SelectOuter =
        withDefaultPos(tpd.Select(qualifier, NameKinds.OuterSelectName(name.toTermName, levels)))
      def copy(original: Tree)(qualifier: Term, name: String, levels: Int): SelectOuter =
        tpd.cpy.Select(original)(qualifier, NameKinds.OuterSelectName(name.toTermName, levels))
      def unapply(x: SelectOuter): Option[(Term, String, Int)] =
        Some((x.qualifier, x.name.toString, x.level))
    end SelectOuter

    object SelectOuterMethodsImpl extends SelectOuterMethods:
      extension (self: SelectOuter):
        def qualifier: Term = self.qualifier
        def name: String = self.name.toString
        def level: Int =
          val NameKinds.OuterSelectName(_, levels) = self.name
          levels
      end extension
    end SelectOuterMethodsImpl

    type While = tpd.WhileDo

    object WhileTypeTestImpl extends TypeTest[Tree, While]:
      def unapply(x: Tree): Option[While & x.type] = x match
        case x: (tpd.WhileDo & x.type) => Some(x)
        case _ => None
    end WhileTypeTestImpl

    object While extends WhileModule:
      def apply(cond: Term, body: Term): While =
        withDefaultPos(tpd.WhileDo(cond, body))
      def copy(original: Tree)(cond: Term, body: Term): While =
        tpd.cpy.WhileDo(original)(cond, body)
      def unapply(x: While): Option[(Term, Term)] =
        Some((x.cond, x.body))
    end While

    object WhileMethodsImpl extends WhileMethods:
      extension (self: While):
        def cond: Term = self.cond
        def body: Term = self.body
      end extension
    end WhileMethodsImpl

    type TypeTree = tpd.Tree

    object TypeTreeTypeTestImpl extends TypeTest[Tree, TypeTree]:
      def unapply(x: Tree): Option[TypeTree & x.type] = x match
        case x: (tpd.TypeBoundsTree & x.type) => None
        case x: (tpd.Tree & x.type) if x.isType => Some(x)
        case _ => None
    end TypeTreeTypeTestImpl

    object TypeTree extends TypeTreeModule:
      def of[T <: AnyKind](using tp: scala.quoted.Type[T]): TypeTree =
        tp.asInstanceOf[TypeImpl].typeTree
    end TypeTree

    object TypeTreeMethodsImpl extends TypeTreeMethods:
      extension (self: TypeTree):
        def tpe: TypeRepr = self.tpe.stripTypeVar
      end extension
    end TypeTreeMethodsImpl

    type Inferred = tpd.TypeTree

    object InferredTypeTestImpl extends TypeTest[Tree, Inferred]:
      def unapply(x: Tree): Option[Inferred & x.type] = x match
        case tpt: (tpd.TypeTree & x.type) if !tpt.tpe.isInstanceOf[Types.TypeBounds] => Some(tpt)
        case _ => None
    end InferredTypeTestImpl

    object Inferred extends InferredModule:
      def apply(tpe: TypeRepr): Inferred =
        withDefaultPos(tpd.TypeTree(tpe))
      def unapply(x: Inferred): Boolean = true
    end Inferred

    type TypeIdent = tpd.Ident

    object TypeIdentTypeTestImpl extends TypeTest[Tree, TypeIdent]:
      def unapply(x: Tree): Option[TypeIdent & x.type] = x match
        case tpt: (tpd.Ident & x.type) if tpt.isType => Some(tpt)
        case _ => None
    end TypeIdentTypeTestImpl

    object TypeIdent extends TypeIdentModule:
      def apply(sym: Symbol): TypeTree =
        assert(sym.isType)
        withDefaultPos(tpd.ref(sym).asInstanceOf[tpd.TypeTree])
      def copy(original: Tree)(name: String): TypeIdent =
        tpd.cpy.Ident(original)(name.toTypeName)
      def unapply(x: TypeIdent): Option[String] =
        Some(x.name.toString)
    end TypeIdent

    object TypeIdentMethodsImpl extends TypeIdentMethods:
      extension (self: TypeIdent):
        def name: String = self.name.toString
      end extension
    end TypeIdentMethodsImpl

    type TypeSelect = tpd.Select

    object TypeSelectTypeTestImpl extends TypeTest[Tree, TypeSelect]:
      def unapply(x: Tree): Option[TypeSelect & x.type] = x match
        case tpt: (tpd.Select & x.type) if tpt.isType && tpt.qualifier.isTerm  => Some(tpt)
        case _ => None
    end TypeSelectTypeTestImpl

    object TypeSelect extends TypeSelectModule:
      def apply(qualifier: Term, name: String): TypeSelect =
        withDefaultPos(tpd.Select(qualifier, name.toTypeName))
      def copy(original: Tree)(qualifier: Term, name: String): TypeSelect =
        tpd.cpy.Select(original)(qualifier, name.toTypeName)
      def unapply(x: TypeSelect): Option[(Term, String)] =
        Some((x.qualifier, x.name.toString))
    end TypeSelect

    object TypeSelectMethodsImpl extends TypeSelectMethods:
      extension (self: TypeSelect):
        def qualifier: Term = self.qualifier
        def name: String = self.name.toString
      end extension
    end TypeSelectMethodsImpl

    type TypeProjection = tpd.Select

    object TypeProjectionTypeTestImpl extends TypeTest[Tree, TypeProjection]:
      def unapply(x: Tree): Option[TypeProjection & x.type] = x match
        case tpt: (tpd.Select & x.type) if tpt.isType && tpt.qualifier.isType => Some(tpt)
        case _ => None
    end TypeProjectionTypeTestImpl

    object TypeProjection extends TypeProjectionModule:
      def copy(original: Tree)(qualifier: TypeTree, name: String): TypeProjection =
        tpd.cpy.Select(original)(qualifier, name.toTypeName)
      def unapply(x: TypeProjection): Option[(TypeTree, String)] =
        Some((x.qualifier, x.name.toString))
    end TypeProjection

    object TypeProjectionMethodsImpl extends TypeProjectionMethods:
      extension (self: TypeProjection):
        def qualifier: TypeTree = self.qualifier
        def name: String = self.name.toString
      end extension
    end TypeProjectionMethodsImpl

    type Singleton = tpd.SingletonTypeTree

    object SingletonTypeTestImpl extends TypeTest[Tree, Singleton]:
      def unapply(x: Tree): Option[Singleton & x.type] = x match
        case tpt: (tpd.SingletonTypeTree & x.type) => Some(tpt)
        case _ => None
    end SingletonTypeTestImpl

    object Singleton extends SingletonModule:
      def apply(ref: Term): Singleton =
        withDefaultPos(tpd.SingletonTypeTree(ref))
      def copy(original: Tree)(ref: Term): Singleton =
        tpd.cpy.SingletonTypeTree(original)(ref)
      def unapply(x: Singleton): Option[Term] =
        Some(x.ref)
    end Singleton

    object SingletonMethodsImpl extends SingletonMethods:
      extension (self: Singleton):
        def ref: Term = self.ref
      end extension
    end SingletonMethodsImpl

    type Refined = tpd.RefinedTypeTree

    object RefinedTypeTestImpl extends TypeTest[Tree, Refined]:
      def unapply(x: Tree): Option[Refined & x.type] = x match
        case tpt: (tpd.RefinedTypeTree & x.type) => Some(tpt)
        case _ => None
    end RefinedTypeTestImpl

    object Refined extends RefinedModule:
      def copy(original: Tree)(tpt: TypeTree, refinements: List[Definition]): Refined =
        tpd.cpy.RefinedTypeTree(original)(tpt, refinements)
      def unapply(x: Refined): Option[(TypeTree, List[Definition])] =
        Some((x.tpt, x.refinements.asInstanceOf[List[Definition]]))
    end Refined

    object RefinedMethodsImpl extends RefinedMethods:
      extension (self: Refined):
        def tpt: TypeTree = self.tpt
        def refinements: List[Definition] = self.refinements.asInstanceOf[List[Definition]]
      end extension
    end RefinedMethodsImpl

    type Applied = tpd.AppliedTypeTree

    object AppliedTypeTestImpl extends TypeTest[Tree, Applied]:
      def unapply(x: Tree): Option[Applied & x.type] = x match
        case tpt: (tpd.AppliedTypeTree & x.type) => Some(tpt)
        case _ => None
    end AppliedTypeTestImpl

    object Applied extends AppliedModule:
      def apply(tpt: TypeTree, args: List[Tree]): Applied =
        withDefaultPos(tpd.AppliedTypeTree(tpt, args))
      def copy(original: Tree)(tpt: TypeTree, args: List[Tree]): Applied =
        tpd.cpy.AppliedTypeTree(original)(tpt, args)
      def unapply(x: Applied): Option[(TypeTree, List[Tree /*TypeTree | TypeBoundsTree*/])] =
        Some((x.tpt, x.args))
    end Applied

    object AppliedMethodsImpl extends AppliedMethods:
      extension (self: Applied):
        def tpt: TypeTree = self.tpt
        def args: List[Tree] = self.args
      end extension
    end AppliedMethodsImpl

    type Annotated = tpd.Annotated

    object AnnotatedTypeTestImpl extends TypeTest[Tree, Annotated]:
      def unapply(x: Tree): Option[Annotated & x.type] = x match
        case tpt: (tpd.Annotated & x.type) => Some(tpt)
        case _ => None
    end AnnotatedTypeTestImpl

    object Annotated extends AnnotatedModule:
      def apply(arg: TypeTree, annotation: Term): Annotated =
        withDefaultPos(tpd.Annotated(arg, annotation))
      def copy(original: Tree)(arg: TypeTree, annotation: Term): Annotated =
        tpd.cpy.Annotated(original)(arg, annotation)
      def unapply(x: Annotated): Option[(TypeTree, Term)] =
        Some((x.arg, x.annotation))
    end Annotated

    object AnnotatedMethodsImpl extends AnnotatedMethods:
      extension (self: Annotated):
        def arg: TypeTree = self.arg
        def annotation: Term = self.annot
      end extension
    end AnnotatedMethodsImpl

    type MatchTypeTree = tpd.MatchTypeTree

    object MatchTypeTreeTypeTestImpl extends TypeTest[Tree, MatchTypeTree]:
      def unapply(x: Tree): Option[MatchTypeTree & x.type] = x match
        case tpt: (tpd.MatchTypeTree & x.type) => Some(tpt)
        case _ => None
    end MatchTypeTreeTypeTestImpl

    object MatchTypeTree extends MatchTypeTreeModule:
      def apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree =
        withDefaultPos(tpd.MatchTypeTree(bound.getOrElse(tpd.EmptyTree), selector, cases))
      def copy(original: Tree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree =
        tpd.cpy.MatchTypeTree(original)(bound.getOrElse(tpd.EmptyTree), selector, cases)
      def unapply(x: MatchTypeTree): Option[(Option[TypeTree], TypeTree, List[TypeCaseDef])] =
        Some((optional(x.bound), x.selector, x.cases))
    end MatchTypeTree

    object MatchTypeTreeMethodsImpl extends MatchTypeTreeMethods:
      extension (self: MatchTypeTree):
        def bound: Option[TypeTree] = optional(self.bound)
        def selector: TypeTree = self.selector
        def cases: List[TypeCaseDef] = self.cases
      end extension
    end MatchTypeTreeMethodsImpl

    type ByName = tpd.ByNameTypeTree

    object ByNameTypeTestImpl extends TypeTest[Tree, ByName]:
      def unapply(x: Tree): Option[ByName & x.type] = x match
        case tpt: (tpd.ByNameTypeTree & x.type) => Some(tpt)
        case _ => None
    end ByNameTypeTestImpl

    object ByName extends ByNameModule:
      def apply(result: TypeTree): ByName =
        withDefaultPos(tpd.ByNameTypeTree(result))
      def copy(original: Tree)(result: TypeTree): ByName =
        tpd.cpy.ByNameTypeTree(original)(result)
      def unapply(x: ByName): Option[TypeTree] =
        Some(x.result)
    end ByName

    object ByNameMethodsImpl extends ByNameMethods:
      extension (self: ByName):
        def result: TypeTree = self.result
      end extension
    end ByNameMethodsImpl

    type LambdaTypeTree = tpd.LambdaTypeTree

    object LambdaTypeTreeTypeTestImpl extends TypeTest[Tree, LambdaTypeTree]:
      def unapply(x: Tree): Option[LambdaTypeTree & x.type] = x match
        case tpt: (tpd.LambdaTypeTree & x.type) => Some(tpt)
        case _ => None
    end LambdaTypeTreeTypeTestImpl

    object LambdaTypeTree extends LambdaTypeTreeModule:
      def apply(tparams: List[TypeDef], body: Tree): LambdaTypeTree =
        withDefaultPos(tpd.LambdaTypeTree(tparams, body))
      def copy(original: Tree)(tparams: List[TypeDef], body: Tree): LambdaTypeTree =
        tpd.cpy.LambdaTypeTree(original)(tparams, body)
      def unapply(tree: LambdaTypeTree): Option[(List[TypeDef], Tree /*TypeTree | TypeBoundsTree*/)] =
        Some((tree.tparams, tree.body))
    end LambdaTypeTree

    object LambdaTypeTreeMethodsImpl extends LambdaTypeTreeMethods:
      extension (self: LambdaTypeTree):
        def tparams: List[TypeDef] = self.tparams
        def body: Tree = self.body
      end extension
    end LambdaTypeTreeMethodsImpl

    type TypeBind = tpd.Bind

    object TypeBindTypeTestImpl extends TypeTest[Tree, TypeBind]:
      def unapply(x: Tree): Option[TypeBind & x.type] = x match
        case tpt: (tpd.Bind & x.type) if tpt.name.isTypeName => Some(tpt)
        case _ => None
    end TypeBindTypeTestImpl

    object TypeBind extends TypeBindModule:
      def copy(original: Tree)(name: String, tpt: Tree): TypeBind =
        tpd.cpy.Bind(original)(name.toTypeName, tpt)
      def unapply(x: TypeBind): Option[(String, Tree /*TypeTree | TypeBoundsTree*/)] =
        Some((x.name.toString, x.body))
    end TypeBind

    object TypeBindMethodsImpl extends TypeBindMethods:
      extension (self: TypeBind):
        def name: String = self.name.toString
        def body: Tree = self.body
      end extension
    end TypeBindMethodsImpl

    type TypeBlock = tpd.Block

    object TypeBlockTypeTestImpl extends TypeTest[Tree, TypeBlock]:
      def unapply(x: Tree): Option[TypeBlock & x.type] = x match
        case tpt: (tpd.Block & x.type) => Some(tpt)
        case _ => None
    end TypeBlockTypeTestImpl

    object TypeBlock extends TypeBlockModule:
      def apply(aliases: List[TypeDef], tpt: TypeTree): TypeBlock =
        withDefaultPos(tpd.Block(aliases, tpt))
      def copy(original: Tree)(aliases: List[TypeDef], tpt: TypeTree): TypeBlock =
        tpd.cpy.Block(original)(aliases, tpt)
      def unapply(x: TypeBlock): Option[(List[TypeDef], TypeTree)] =
        Some((x.aliases, x.tpt))
    end TypeBlock

    object TypeBlockMethodsImpl extends TypeBlockMethods:
      extension (self: TypeBlock):
        def aliases: List[TypeDef] = self.stats.map { case alias: TypeDef => alias }
        def tpt: TypeTree = self.expr
      end extension
    end TypeBlockMethodsImpl

    type TypeBoundsTree = tpd.TypeBoundsTree | tpd.TypeTree

    object TypeBoundsTreeTypeTestImpl extends TypeTest[Tree, TypeBoundsTree]:
      def unapply(x: Tree): Option[TypeBoundsTree & x.type] = x match
        case x: (tpd.TypeBoundsTree & x.type) => Some(x)
        case x: (tpd.TypeTree & x.type) =>
          x.tpe match
            case tpe: Types.TypeBounds => Some(x)
            case _ => None
        case _ => None
    end TypeBoundsTreeTypeTestImpl

    object TypeBoundsTree extends TypeBoundsTreeModule:
      def apply(low: TypeTree, hi: TypeTree): TypeBoundsTree =
        withDefaultPos(tpd.TypeBoundsTree(low, hi))
      def copy(original: Tree)(low: TypeTree, hi: TypeTree): TypeBoundsTree =
        tpd.cpy.TypeBoundsTree(original)(low, hi, tpd.EmptyTree)
      def unapply(x: TypeBoundsTree): Option[(TypeTree, TypeTree)] =
        Some((x.low, x.hi))
    end TypeBoundsTree

    object TypeBoundsTreeMethodsImpl extends TypeBoundsTreeMethods:
      extension (self: TypeBoundsTree):
        def tpe: TypeBounds = self.tpe.asInstanceOf[Types.TypeBounds]
        def low: TypeTree = self match
          case self: tpd.TypeBoundsTree => self.lo
          case self: tpd.TypeTree => tpd.TypeTree(self.tpe.asInstanceOf[Types.TypeBounds].lo).withSpan(self.span)
        def hi: TypeTree = self match
          case self: tpd.TypeBoundsTree => self.hi
          case self: tpd.TypeTree => tpd.TypeTree(self.tpe.asInstanceOf[Types.TypeBounds].hi).withSpan(self.span)
      end extension
    end TypeBoundsTreeMethodsImpl

    type WildcardTypeTree = tpd.Ident

    object WildcardTypeTreeTypeTestImpl extends TypeTest[Tree, WildcardTypeTree]:
      def unapply(x: Tree): Option[WildcardTypeTree & x.type] = x match
        case x: (tpd.Ident & x.type) if x.name == nme.WILDCARD => Some(x)
        case _ => None
    end WildcardTypeTreeTypeTestImpl

    object WildcardTypeTree extends WildcardTypeTreeModule:
      def apply(tpe: TypeRepr): WildcardTypeTree = withDefaultPos(tpd.Underscore(tpe))
      def unapply(x: WildcardTypeTree): Boolean = true
    end WildcardTypeTree

    object WildcardTypeTreeMethodsImpl extends WildcardTypeTreeMethods:
      extension (self: WildcardTypeTree):
        def tpe: TypeRepr = self.tpe.stripTypeVar
      end extension
    end WildcardTypeTreeMethodsImpl

    type CaseDef = tpd.CaseDef

    object CaseDefTypeTestImpl extends TypeTest[Tree, CaseDef]:
      def unapply(x: Tree): Option[CaseDef & x.type] = x match
        case tree: (tpd.CaseDef & x.type) if tree.body.isTerm => Some(tree)
        case _ => None
    end CaseDefTypeTestImpl

    object CaseDef extends CaseDefModule:
      def apply(pattern: Tree, guard: Option[Term], rhs: Term): CaseDef =
        tpd.CaseDef(pattern, guard.getOrElse(tpd.EmptyTree), rhs)
      def copy(original: Tree)(pattern: Tree, guard: Option[Term], rhs: Term): CaseDef =
        tpd.cpy.CaseDef(original)(pattern, guard.getOrElse(tpd.EmptyTree), rhs)
      def unapply(x: CaseDef): Option[(Tree, Option[Term], Term)] =
        Some((x.pat, optional(x.guard), x.body))
    end CaseDef

    object CaseDefMethodsImpl extends CaseDefMethods:
      extension (self: CaseDef):
        def pattern: Tree = self.pat
        def guard: Option[Term] = optional(self.guard)
        def rhs: Term = self.body
      end extension
    end CaseDefMethodsImpl

    type TypeCaseDef = tpd.CaseDef

    object TypeCaseDefTypeTestImpl extends TypeTest[Tree, TypeCaseDef]:
      def unapply(x: Tree): Option[TypeCaseDef & x.type] = x match
        case tree: (tpd.CaseDef & x.type) if tree.body.isType => Some(tree)
        case _ => None
    end TypeCaseDefTypeTestImpl

    object TypeCaseDef extends TypeCaseDefModule:
      def apply(pattern: TypeTree, rhs: TypeTree): TypeCaseDef =
        tpd.CaseDef(pattern, tpd.EmptyTree, rhs)
      def copy(original: Tree)(pattern: TypeTree, rhs: TypeTree): TypeCaseDef =
        tpd.cpy.CaseDef(original)(pattern, tpd.EmptyTree, rhs)
      def unapply(tree: TypeCaseDef): Option[(TypeTree, TypeTree)] =
        Some((tree.pat, tree.body))
    end TypeCaseDef

    object TypeCaseDefMethodsImpl extends TypeCaseDefMethods:
      extension (self: TypeCaseDef):
        def pattern: TypeTree = self.pat
        def rhs: TypeTree = self.body
      end extension
    end TypeCaseDefMethodsImpl

    type Bind = tpd.Bind

    object BindTypeTestImpl extends TypeTest[Tree, Bind]:
      def unapply(x: Tree): Option[Bind & x.type] = x match
        case x: (tpd.Bind & x.type) if x.name.isTermName => Some(x)
        case _ => None
    end BindTypeTestImpl

    object Bind extends BindModule:
      def apply(sym: Symbol, pattern: Tree): Bind =
        tpd.Bind(sym, pattern)
      def copy(original: Tree)(name: String, pattern: Tree): Bind =
        withDefaultPos(tpd.cpy.Bind(original)(name.toTermName, pattern))
      def unapply(pattern: Bind): Option[(String, Tree)] =
        Some((pattern.name.toString, pattern.pattern))
    end Bind

    object BindMethodsImpl extends BindMethods:
      extension (self: Bind):
        def name: String = self.name.toString
        def pattern: Tree = self.body
      end extension
    end BindMethodsImpl

    type Unapply = tpd.UnApply | tpd.Typed // tpd.Typed containing a tpd.UnApply as expression

    object UnapplyTypeTestImpl extends TypeTest[Tree, Unapply]:
      def unapply(x: Tree): Option[Unapply & x.type] =
        x match // keep in sync with UnapplyMethodsImpl.selfUnApply
          case x: (tpd.UnApply & x.type) => Some(x)
          case x: (tpd.Typed & x.type) if x.expr.isInstanceOf[tpd.UnApply] => Some(x)
          case _ => None
    end UnapplyTypeTestImpl

    object Unapply extends UnapplyModule:
      def copy(original: Tree)(fun: Term, implicits: List[Term], patterns: List[Tree]): Unapply =
        withDefaultPos(tpd.cpy.UnApply(original)(fun, implicits, patterns))
      def unapply(x: Unapply): Option[(Term, List[Term], List[Tree])] =
        Some((x.fun, x.implicits, x.patterns))
    end Unapply

    object UnapplyMethodsImpl extends UnapplyMethods:
      extension (self: Unapply):
        def fun: Term = selfUnApply(self).fun
        def implicits: List[Term] = selfUnApply(self).implicits
        def patterns: List[Tree] = effectivePatterns(selfUnApply(self).patterns)
      end extension
      private def selfUnApply(self: Unapply): tpd.UnApply =
        self match // keep in sync with UnapplyTypeTest
          case self: tpd.UnApply => self
          case self: tpd.Typed => self.expr.asInstanceOf[tpd.UnApply]
      private def effectivePatterns(patterns: List[Tree]): List[Tree] =
        patterns match
          case patterns0 :+ dotc.ast.Trees.SeqLiteral(elems, _) => patterns0 ::: elems
          case _ => patterns
    end UnapplyMethodsImpl

    type Alternatives = tpd.Alternative

    object AlternativesTypeTestImpl extends TypeTest[Tree, Alternatives]:
      def unapply(x: Tree): Option[Alternatives & x.type] = x match
        case x: (tpd.Alternative & x.type) => Some(x)
        case _ => None
    end AlternativesTypeTestImpl

    object Alternatives extends AlternativesModule:
      def apply(patterns: List[Tree]): Alternatives =
        withDefaultPos(tpd.Alternative(patterns))
      def copy(original: Tree)(patterns: List[Tree]): Alternatives =
        tpd.cpy.Alternative(original)(patterns)
      def unapply(x: Alternatives): Option[List[Tree]] =
        Some(x.patterns)
    end Alternatives

    object AlternativesMethodsImpl extends AlternativesMethods:
      extension (self: Alternatives):
        def patterns: List[Tree] = self.trees
      end extension
    end AlternativesMethodsImpl

    type ImportSelector = untpd.ImportSelector

    object ImportSelector extends ImportSelectorModule

    type SimpleSelector = untpd.ImportSelector

    object SimpleSelectorTypeTestImpl extends TypeTest[ImportSelector, SimpleSelector]:
      def unapply(x: ImportSelector): Option[SimpleSelector & x.type] = x match
        case x: (untpd.ImportSelector & x.type) if x.renamed.isEmpty && !x.isGiven => Some(x)
        case _ => None // TODO: handle import bounds
    end SimpleSelectorTypeTestImpl

    object SimpleSelector extends SimpleSelectorModule:
      def unapply(x: SimpleSelector): Option[String] = Some(x.name.toString)
    end SimpleSelector


    object SimpleSelectorMethodsImpl extends SimpleSelectorMethods:
      extension (self: SimpleSelector):
        def name: String = self.imported.name.toString
        def namePos: Position = self.imported.sourcePos
      end extension
    end SimpleSelectorMethodsImpl

    type RenameSelector = untpd.ImportSelector

    object RenameSelectorTypeTestImpl extends TypeTest[ImportSelector, RenameSelector]:
      def unapply(x: ImportSelector): Option[RenameSelector & x.type] = x match
        case x: (untpd.ImportSelector & x.type) if !x.renamed.isEmpty => Some(x)
        case _ => None
    end RenameSelectorTypeTestImpl

    object RenameSelector extends RenameSelectorModule:
      def unapply(x: RenameSelector): Option[(String, String)] = Some((x.fromName, x.toName))
    end RenameSelector

    object RenameSelectorMethodsImpl extends RenameSelectorMethods:
      extension (self: RenameSelector):
        def fromName: String = self.imported.name.toString
        def fromPos: Position = self.imported.sourcePos
        def toName: String = self.renamed.asInstanceOf[untpd.Ident].name.toString
        def toPos: Position = self.renamed.asInstanceOf[untpd.Ident].sourcePos
      end extension
    end RenameSelectorMethodsImpl

    type OmitSelector = untpd.ImportSelector

    object OmitSelectorTypeTestImpl extends TypeTest[ImportSelector, OmitSelector]:
      def unapply(x: ImportSelector): Option[OmitSelector & x.type] = x match {
        case self: (untpd.ImportSelector & x.type) =>
          self.renamed match
            case dotc.ast.Trees.Ident(nme.WILDCARD) => Some(self)
            case _ => None
        case _ => None
      }
    end OmitSelectorTypeTestImpl

    object OmitSelector extends OmitSelectorModule:
      def unapply(x: OmitSelector): Option[String] = Some(x.imported.name.toString)
    end OmitSelector

    object OmitSelectorMethodsImpl extends OmitSelectorMethods:
      extension (self: OmitSelector):
        def name: String = self.imported.toString
        def namePos: Position = self.imported.sourcePos
      end extension
    end OmitSelectorMethodsImpl

    type GivenSelector = untpd.ImportSelector

    object GivenSelectorTypeTestImpl extends TypeTest[ImportSelector, GivenSelector]:
      def unapply(x: ImportSelector): Option[GivenSelector & x.type] = x match {
        case self: (untpd.ImportSelector & x.type) if x.isGiven => Some(self)
        case _ => None
      }
    end GivenSelectorTypeTestImpl

    object GivenSelector extends GivenSelectorModule:
      def unapply(x: GivenSelector): Option[Option[TypeTree]] =
        Some(GivenSelectorMethodsImpl.bound(x))
    end GivenSelector

    object GivenSelectorMethodsImpl extends GivenSelectorMethods:
      extension (self: GivenSelector):
        def bound: Option[TypeTree] =
          self.bound match
            case untpd.TypedSplice(tpt) => Some(tpt)
            case _ => None
      end extension
    end GivenSelectorMethodsImpl

    type TypeRepr = dotc.core.Types.Type

    object TypeRepr extends TypeReprModule:
      def of[T <: AnyKind](using tp: scala.quoted.Type[T]): TypeRepr =
        tp.asInstanceOf[TypeImpl].typeTree.tpe
      def typeConstructorOf(clazz: Class[?]): TypeRepr =
        if (clazz.isPrimitive)
          if (clazz == classOf[Boolean]) dotc.core.Symbols.defn.BooleanType
          else if (clazz == classOf[Byte]) dotc.core.Symbols.defn.ByteType
          else if (clazz == classOf[Char]) dotc.core.Symbols.defn.CharType
          else if (clazz == classOf[Short]) dotc.core.Symbols.defn.ShortType
          else if (clazz == classOf[Int]) dotc.core.Symbols.defn.IntType
          else if (clazz == classOf[Long]) dotc.core.Symbols.defn.LongType
          else if (clazz == classOf[Float]) dotc.core.Symbols.defn.FloatType
          else if (clazz == classOf[Double]) dotc.core.Symbols.defn.DoubleType
          else dotc.core.Symbols.defn.UnitType
        else if (clazz.isArray)
          dotc.core.Symbols.defn.ArrayType.appliedTo(typeConstructorOf(clazz.getComponentType))
        else if clazz.isMemberClass then
          val name = clazz.getSimpleName.toTypeName
          val enclosing = typeConstructorOf(clazz.getEnclosingClass)
          if (enclosing.member(name).exists) enclosing.select(name)
          else enclosing.classSymbol.companionModule.termRef.select(name)
        else
          dotc.core.Symbols.getClassIfDefined(clazz.getCanonicalName).typeRef
    end TypeRepr

    object TypeReprMethodsImpl extends TypeReprMethods:
      extension (self: TypeRepr):
        def showExtractors: String =
          Extractors.showType(using QuotesImpl.this)(self)

        def show: String =
          SourceCode.showType(using QuotesImpl.this)(self)(SyntaxHighlight.plain, fullNames = true)

        def showShort: String =
          SourceCode.showType(using QuotesImpl.this)(self)(SyntaxHighlight.plain, fullNames = false)

        def showAnsiColored: String =
          SourceCode.showType(using QuotesImpl.this)(self)(SyntaxHighlight.ANSI, fullNames = true)

        def seal: scala.quoted.Type[_] = self.asType

        def asType: scala.quoted.Type[?] =
          new TypeImpl(Inferred(self), QuotesImpl.this.hashCode)

        def =:=(that: TypeRepr): Boolean = self =:= that
        def <:<(that: TypeRepr): Boolean = self <:< that
        def widen: TypeRepr = self.widen
        def widenTermRefExpr: TypeRepr = self.widenTermRefExpr
        def dealias: TypeRepr = self.dealias
        def simplified: TypeRepr = self.simplified
        def classSymbol: Option[Symbol] =
          if self.classSymbol.exists then Some(self.classSymbol.asClass)
          else None
        def typeSymbol: Symbol = self.typeSymbol
        def termSymbol: Symbol = self.termSymbol
        def isSingleton: Boolean = self.isSingleton
        def memberType(member: Symbol): TypeRepr =
          member.info.asSeenFrom(self, member.owner)
        def baseClasses: List[Symbol] = self.baseClasses
        def baseType(cls: Symbol): TypeRepr = self.baseType(cls)
        def derivesFrom(cls: Symbol): Boolean = self.derivesFrom(cls)
        def isFunctionType: Boolean =
          dotc.core.Symbols.defn.isFunctionType(self)
        def isContextFunctionType: Boolean =
          dotc.core.Symbols.defn.isContextFunctionType(self)
        def isErasedFunctionType: Boolean =
          dotc.core.Symbols.defn.isErasedFunctionType(self)
        def isDependentFunctionType: Boolean =
          val tpNoRefinement = self.dropDependentRefinement
          tpNoRefinement != self
          && dotc.core.Symbols.defn.isNonRefinedFunction(tpNoRefinement)
        def select(sym: Symbol): TypeRepr = self.select(sym)
        def appliedTo(targ: TypeRepr): TypeRepr =
          dotc.core.Types.decorateTypeApplications(self).appliedTo(targ)
        def appliedTo(targs: List[TypeRepr]): TypeRepr =
          dotc.core.Types.decorateTypeApplications(self).appliedTo(targs)
      end extension
    end TypeReprMethodsImpl

    type ConstantType = dotc.core.Types.ConstantType

    object ConstantTypeTypeTestImpl extends TypeTest[TypeRepr, ConstantType]:
      def unapply(x: TypeRepr): Option[ConstantType & x.type] = x match
        case tpe: (Types.ConstantType & x.type) => Some(tpe)
        case _ => None
    end ConstantTypeTypeTestImpl

    object ConstantType extends ConstantTypeModule:
      def apply(const: Constant): ConstantType = Types.ConstantType(const)
      def unapply(x: ConstantType): Option[Constant] = Some(x.constant)
    end ConstantType

    object ConstantTypeMethodsImpl extends ConstantTypeMethods:
      extension (self: ConstantType) def constant: Constant = self.value
    end ConstantTypeMethodsImpl

    type TermRef = dotc.core.Types.NamedType

    object TermRefTypeTestImpl extends TypeTest[TypeRepr, TermRef]:
      def unapply(x: TypeRepr): Option[TermRef & x.type] = x match
        case tpe: (Types.TermRef & x.type) => Some(tpe)
        case _ => None
    end TermRefTypeTestImpl

    object TermRef extends TermRefModule:
      def apply(qual: TypeRepr, name: String): TermRef =
        Types.TermRef(qual, name.toTermName)
      def unapply(x: TermRef): Option[(TypeRepr, String)] =
        Some((x.prefix, x.name.toString))
    end TermRef

    object TermRefMethodsImpl extends TermRefMethods:
      extension (self: TermRef):
        def qualifier: TypeRepr = self.prefix
        def name: String = self.name.toString
      end extension
    end TermRefMethodsImpl

    type TypeRef = dotc.core.Types.NamedType

    object TypeRefTypeTestImpl extends TypeTest[TypeRepr, TypeRef]:
      def unapply(x: TypeRepr): Option[TypeRef & x.type] = x match
        case tpe: (Types.TypeRef & x.type) => Some(tpe)
        case _ => None
    end TypeRefTypeTestImpl

    object TypeRef extends TypeRefModule:
      def unapply(x: TypeRef): Option[(TypeRepr, String)] =
        Some((x.prefix, x.name.toString))
    end TypeRef

    object TypeRefMethodsImpl extends TypeRefMethods:
      extension (self: TypeRef):
        def qualifier: TypeRepr = self.prefix
        def name: String = self.name.toString
        def isOpaqueAlias: Boolean = self.symbol.isOpaqueAlias
        def translucentSuperType: TypeRepr = self.translucentSuperType
      end extension
    end TypeRefMethodsImpl

    type SuperType = dotc.core.Types.SuperType

    object SuperTypeTypeTestImpl extends TypeTest[TypeRepr, SuperType]:
      def unapply(x: TypeRepr): Option[SuperType & x.type] = x match
        case tpe: (Types.SuperType & x.type) => Some(tpe)
        case _ => None
    end SuperTypeTypeTestImpl

    object SuperType extends SuperTypeModule:
      def apply(thistpe: TypeRepr, supertpe: TypeRepr): SuperType =
        Types.SuperType(thistpe, supertpe)
      def unapply(x: SuperType): Option[(TypeRepr, TypeRepr)] =
        Some((x.thistpe, x.supertpe))
    end SuperType

    object SuperTypeMethodsImpl extends SuperTypeMethods:
      extension (self: SuperType):
        def thistpe: TypeRepr = self.thistpe
        def supertpe: TypeRepr = self.thistpe
      end extension
    end SuperTypeMethodsImpl

    type Refinement = dotc.core.Types.RefinedType

    object RefinementTypeTestImpl extends TypeTest[TypeRepr, Refinement]:
      def unapply(x: TypeRepr): Option[Refinement & x.type] = x match
        case tpe: (Types.RefinedType & x.type) => Some(tpe)
        case _ => None
    end RefinementTypeTestImpl

    object Refinement extends RefinementModule:
      def apply(parent: TypeRepr, name: String, info: TypeRepr): Refinement =
        val name1 =
          info match
            case _: TypeBounds => name.toTypeName
            case _ => name.toTermName
        Types.RefinedType(parent, name1, info)
      def unapply(x: Refinement): Option[(TypeRepr, String, TypeRepr)] =
        Some((x.parent, x.name, x.info))
    end Refinement

    object RefinementMethodsImpl extends RefinementMethods:
      extension (self: Refinement):
        def parent: TypeRepr = self.parent
        def name: String = self.refinedName.toString
        def info: TypeRepr = self.refinedInfo
      end extension
    end RefinementMethodsImpl

    type AppliedType = dotc.core.Types.AppliedType

    object AppliedTypeTypeTestImpl extends TypeTest[TypeRepr, AppliedType]:
      def unapply(x: TypeRepr): Option[AppliedType & x.type] = x match
        case tpe: (Types.AppliedType & x.type) => Some(tpe)
        case _ => None
    end AppliedTypeTypeTestImpl

    object AppliedType extends AppliedTypeModule:
      def unapply(x: AppliedType): Option[(TypeRepr, List[TypeRepr])] =
        Some((x.tycon, x.args))
    end AppliedType

    object AppliedTypeMethodsImpl extends AppliedTypeMethods:
      extension (self: AppliedType):
        def tycon: TypeRepr = self.tycon
        def args: List[TypeRepr] = self.args
      end extension
    end AppliedTypeMethodsImpl

    type AnnotatedType = dotc.core.Types.AnnotatedType

    object AnnotatedTypeTypeTestImpl extends TypeTest[TypeRepr, AnnotatedType]:
      def unapply(x: TypeRepr): Option[AnnotatedType & x.type] = x match
        case tpe: (Types.AnnotatedType & x.type) => Some(tpe)
        case _ => None
    end AnnotatedTypeTypeTestImpl

    object AnnotatedType extends AnnotatedTypeModule:
      def apply(underlying: TypeRepr, annot: Term): AnnotatedType =
        Types.AnnotatedType(underlying, Annotations.Annotation(annot))
      def unapply(x: AnnotatedType): Option[(TypeRepr, Term)] =
        Some((x.underlying.stripTypeVar, x.annot.tree))
    end AnnotatedType

    object AnnotatedTypeMethodsImpl extends AnnotatedTypeMethods:
      extension (self: AnnotatedType):
        def underlying: TypeRepr = self.underlying.stripTypeVar
        def annot: Term = self.annot.tree
      end extension
    end AnnotatedTypeMethodsImpl

    type AndType = dotc.core.Types.AndType

    object AndTypeTypeTestImpl extends TypeTest[TypeRepr, AndType]:
      def unapply(x: TypeRepr): Option[AndType & x.type] = x match
        case tpe: (Types.AndType & x.type) => Some(tpe)
        case _ => None
    end AndTypeTypeTestImpl

    object AndType extends AndTypeModule:
      def apply(lhs: TypeRepr, rhs: TypeRepr): AndType = Types.AndType(lhs, rhs)
      def unapply(x: AndType): Option[(TypeRepr, TypeRepr)] = Some((x.left, x.right))
    end AndType

    object AndTypeMethodsImpl extends AndTypeMethods:
      extension (self: AndType):
        def left: TypeRepr = self.tp1.stripTypeVar
        def right: TypeRepr = self.tp2.stripTypeVar
      end extension
    end AndTypeMethodsImpl

    type OrType = dotc.core.Types.OrType

    object OrTypeTypeTestImpl extends TypeTest[TypeRepr, OrType]:
      def unapply(x: TypeRepr): Option[OrType & x.type] = x match
        case tpe: (Types.OrType & x.type) => Some(tpe)
        case _ => None
    end OrTypeTypeTestImpl

    object OrType extends OrTypeModule:
      def apply(lhs: TypeRepr, rhs: TypeRepr): OrType = Types.OrType(lhs, rhs, soft = false)
      def unapply(x: OrType): Option[(TypeRepr, TypeRepr)] = Some((x.left, x.right))
    end OrType

    object OrTypeMethodsImpl extends OrTypeMethods:
      extension (self: OrType):
        def left: TypeRepr = self.tp1.stripTypeVar
        def right: TypeRepr = self.tp2.stripTypeVar
      end extension
    end OrTypeMethodsImpl

    type MatchType = dotc.core.Types.MatchType

    object MatchTypeTypeTestImpl extends TypeTest[TypeRepr, MatchType]:
      def unapply(x: TypeRepr): Option[MatchType & x.type] = x match
        case tpe: (Types.MatchType & x.type) => Some(tpe)
        case _ => None
    end MatchTypeTypeTestImpl

    object MatchType extends MatchTypeModule:
      def apply(bound: TypeRepr, scrutinee: TypeRepr, cases: List[TypeRepr]): MatchType =
        Types.MatchType(bound, scrutinee, cases)
      def unapply(x: MatchType): Option[(TypeRepr, TypeRepr, List[TypeRepr])] =
        Some((x.bound, x.scrutinee, x.cases))
    end MatchType

    object MatchTypeMethodsImpl extends MatchTypeMethods:
      extension (self: MatchType):
        def bound: TypeRepr = self.bound
        def scrutinee: TypeRepr = self.scrutinee
        def cases: List[TypeRepr] = self.cases
      end extension
    end MatchTypeMethodsImpl

    type ByNameType = dotc.core.Types.ExprType

    object ByNameTypeTypeTestImpl extends TypeTest[TypeRepr, ByNameType]:
      def unapply(x: TypeRepr): Option[ByNameType & x.type] = x match
        case tpe: (Types.ExprType & x.type) => Some(tpe)
        case _ => None
    end ByNameTypeTypeTestImpl

    object ByNameType extends ByNameTypeModule:
      def apply(underlying: TypeRepr): TypeRepr = Types.ExprType(underlying)
      def unapply(x: ByNameType): Option[TypeRepr] = Some(x.underlying)
    end ByNameType

    object ByNameTypeMethodsImpl extends ByNameTypeMethods:
      extension (self: ByNameType):
        def underlying: TypeRepr = self.resType.stripTypeVar
      end extension
    end ByNameTypeMethodsImpl

    type ParamRef = dotc.core.Types.ParamRef

    object ParamRefTypeTestImpl extends TypeTest[TypeRepr, ParamRef]:
      def unapply(x: TypeRepr): Option[ParamRef & x.type] = x match
        case tpe: (Types.TypeParamRef & x.type) => Some(tpe)
        case tpe: (Types.TermParamRef & x.type) => Some(tpe)
        case _ => None
    end ParamRefTypeTestImpl

    object ParamRef extends ParamRefModule:
      def unapply(x: ParamRef): Option[(LambdaType, Int)] =
        Some((x.binder, x.paramNum))
    end ParamRef

    object ParamRefMethodsImpl extends ParamRefMethods:
      extension (self: ParamRef):
        def binder: LambdaType = self.binder.asInstanceOf[LambdaType] // Cast to tpd
        def paramNum: Int = self.paramNum
      end extension
    end ParamRefMethodsImpl

    type ThisType = dotc.core.Types.ThisType

    object ThisTypeTypeTestImpl extends TypeTest[TypeRepr, ThisType]:
      def unapply(x: TypeRepr): Option[ThisType & x.type] = x match
        case tpe: (Types.ThisType & x.type) => Some(tpe)
        case _ => None
    end ThisTypeTypeTestImpl

    object ThisType extends ThisTypeModule:
      def unapply(x: ThisType): Option[TypeRepr] = Some(x.tref)
    end ThisType

    object ThisTypeMethodsImpl extends ThisTypeMethods:
      extension (self: ThisType):
        def tref: TypeRepr = self.tref
      end extension
    end ThisTypeMethodsImpl

    type RecursiveThis = dotc.core.Types.RecThis

    object RecursiveThisTypeTestImpl extends TypeTest[TypeRepr, RecursiveThis]:
      def unapply(x: TypeRepr): Option[RecursiveThis & x.type] = x match
        case tpe: (Types.RecThis & x.type) => Some(tpe)
        case _ => None
    end RecursiveThisTypeTestImpl

    object RecursiveThis extends RecursiveThisModule:
      def unapply(x: RecursiveThis): Option[RecursiveType] = Some(x.binder)
    end RecursiveThis


    object RecursiveThisMethodsImpl extends RecursiveThisMethods:
      extension (self: RecursiveThis):
        def binder: RecursiveType = self.binder
      end extension
    end RecursiveThisMethodsImpl

    type RecursiveType = dotc.core.Types.RecType

    object RecursiveTypeTypeTestImpl extends TypeTest[TypeRepr, RecursiveType]:
      def unapply(x: TypeRepr): Option[RecursiveType & x.type] = x match
        case tpe: (Types.RecType & x.type) => Some(tpe)
        case _ => None
    end RecursiveTypeTypeTestImpl

    object RecursiveType extends RecursiveTypeModule:
      def apply(parentExp: RecursiveType => TypeRepr): RecursiveType =
        Types.RecType(parentExp)
      def unapply(x: RecursiveType): Option[TypeRepr] = Some(x.underlying)
    end RecursiveType

    object RecursiveTypeMethodsImpl extends RecursiveTypeMethods:
      extension (self: RecursiveType):
        def underlying: TypeRepr = self.underlying.stripTypeVar
        def recThis: RecursiveThis = self.recThis
      end extension
    end RecursiveTypeMethodsImpl

    type LambdaType = dotc.core.Types.LambdaType

    type MethodType = dotc.core.Types.MethodType

    object MethodTypeTypeTestImpl extends TypeTest[TypeRepr, MethodType]:
      def unapply(x: TypeRepr): Option[MethodType & x.type] = x match
        case tpe: (Types.MethodType & x.type) => Some(tpe)
        case _ => None
    end MethodTypeTypeTestImpl

    object MethodType extends MethodTypeModule:
      def apply(paramNames: List[String])(paramInfosExp: MethodType => List[TypeRepr], resultTypeExp: MethodType => TypeRepr): MethodType =
        Types.MethodType(paramNames.map(_.toTermName))(paramInfosExp, resultTypeExp)
      def unapply(x: MethodType): Option[(List[String], List[TypeRepr], TypeRepr)] =
        Some((x.paramNames.map(_.toString), x.paramTypes, x.resType))
    end MethodType

    object MethodTypeMethodsImpl extends MethodTypeMethods:
      extension (self: MethodType):
        def isErased: Boolean = self.isErasedMethod
        def isImplicit: Boolean = self.isImplicitMethod
        def param(idx: Int): TypeRepr = self.newParamRef(idx)
        def paramNames: List[String] = self.paramNames.map(_.toString)
        def paramTypes: List[TypeRepr] = self.paramInfos
        def resType: TypeRepr = self.resType
      end extension
    end MethodTypeMethodsImpl

    type PolyType = dotc.core.Types.PolyType

    object PolyTypeTypeTestImpl extends TypeTest[TypeRepr, PolyType]:
      def unapply(x: TypeRepr): Option[PolyType & x.type] = x match
        case tpe: (Types.PolyType & x.type) => Some(tpe)
        case _ => None
    end PolyTypeTypeTestImpl

    object PolyType extends PolyTypeModule:
      def apply(paramNames: List[String])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => TypeRepr): PolyType =
        Types.PolyType(paramNames.map(_.toTypeName))(paramBoundsExp, resultTypeExp)
      def unapply(x: PolyType): Option[(List[String], List[TypeBounds], TypeRepr)] =
        Some((x.paramNames.map(_.toString), x.paramBounds, x.resType))
    end PolyType

    object PolyTypeMethodsImpl extends PolyTypeMethods:
      extension (self: PolyType):
        def param(idx: Int): TypeRepr = self.newParamRef(idx)
        def paramNames: List[String] = self.paramNames.map(_.toString)
        def paramBounds: List[TypeBounds] = self.paramInfos
        def resType: TypeRepr = self.resType
      end extension
    end PolyTypeMethodsImpl

    type TypeLambda = dotc.core.Types.TypeLambda

    object TypeLambdaTypeTestImpl extends TypeTest[TypeRepr, TypeLambda]:
      def unapply(x: TypeRepr): Option[TypeLambda & x.type] = x match
        case tpe: (Types.TypeLambda & x.type) => Some(tpe)
        case _ => None
    end TypeLambdaTypeTestImpl

    object TypeLambda extends TypeLambdaModule:
      def apply(paramNames: List[String], boundsFn: TypeLambda => List[TypeBounds], bodyFn: TypeLambda => TypeRepr): TypeLambda =
        Types.HKTypeLambda(paramNames.map(_.toTypeName))(boundsFn, bodyFn)
      def unapply(x: TypeLambda): Option[(List[String], List[TypeBounds], TypeRepr)] =
        Some((x.paramNames.map(_.toString), x.paramBounds, x.resType))
    end TypeLambda

    object TypeLambdaMethodsImpl extends TypeLambdaMethods:
      extension (self: TypeLambda):
        def paramNames: List[String] = self.paramNames.map(_.toString)
        def paramBounds: List[TypeBounds] = self.paramInfos
        def param(idx: Int): TypeRepr = self.newParamRef(idx)
        def resType: TypeRepr = self.resType
      end extension
    end TypeLambdaMethodsImpl

    type TypeBounds = dotc.core.Types.TypeBounds

    object TypeBoundsTypeTestImpl extends TypeTest[TypeRepr, TypeBounds]:
      def unapply(x: TypeRepr): Option[TypeBounds & x.type] = x match
        case x: (Types.TypeBounds & x.type) => Some(x)
        case _ => None
    end TypeBoundsTypeTestImpl

    object TypeBounds extends TypeBoundsModule:
      def apply(low: TypeRepr, hi: TypeRepr): TypeBounds = Types.TypeBounds(low, hi)
      def unapply(x: TypeBounds): Option[(TypeRepr, TypeRepr)] = Some((x.low.stripLazyRef, x.hi.stripLazyRef))
      def empty: TypeBounds = Types .TypeBounds.empty
      def upper(hi: TypeRepr): TypeBounds = Types .TypeBounds.upper(hi)
      def lower(lo: TypeRepr): TypeBounds = Types .TypeBounds.lower(lo)
    end TypeBounds

    object TypeBoundsMethodsImpl extends TypeBoundsMethods:
      extension (self: TypeBounds):
        def low: TypeRepr = self.lo.stripLazyRef
        def hi: TypeRepr = self.hi.stripLazyRef
      end extension
    end TypeBoundsMethodsImpl

    type NoPrefix = dotc.core.Types.NoPrefix.type

    object NoPrefixTypeTestImpl extends TypeTest[TypeRepr, NoPrefix]:
      def unapply(x: TypeRepr): Option[NoPrefix & x.type] =
        if x == Types.NoPrefix then Some(x.asInstanceOf[NoPrefix & x.type]) else None
    end NoPrefixTypeTestImpl

    object NoPrefix extends NoPrefixModule:
      def unapply(x: NoPrefix): Boolean = true
    end NoPrefix

    type Constant = dotc.core.Constants.Constant

    object Constant extends ConstantModule:

      object Boolean extends BooleanModule:
        def apply(x: Boolean): Constant = dotc.core.Constants.Constant(x)
        def unapply(constant: Constant): Option[Boolean] =
          if constant.tag == dotc.core.Constants.BooleanTag then Some(constant.booleanValue)
          else None
      end Boolean

      object Byte extends ByteModule:
        def apply(x: Byte): Constant = dotc.core.Constants.Constant(x)
        def unapply(constant: Constant): Option[Byte] =
          if constant.tag == dotc.core.Constants.ByteTag then Some(constant.byteValue)
          else None
      end Byte

      object Short extends ShortModule:
        def apply(x: Short): Constant = dotc.core.Constants.Constant(x)
        def unapply(constant: Constant): Option[Short] =
          if constant.tag == dotc.core.Constants.ShortTag then Some(constant.shortValue)
          else None
      end Short

      object Int extends IntModule:
        def apply(x: Int): Constant = dotc.core.Constants.Constant(x)
        def unapply(constant: Constant): Option[Int] =
          if constant.tag == dotc.core.Constants.IntTag then Some(constant.intValue)
          else None
      end Int

      object Long extends LongModule:
        def apply(x: Long): Constant = dotc.core.Constants.Constant(x)
        def unapply(constant: Constant): Option[Long] =
          if constant.tag == dotc.core.Constants.LongTag then Some(constant.longValue)
          else None
      end Long

      object Float extends FloatModule:
        def apply(x: Float): Constant = dotc.core.Constants.Constant(x)
        def unapply(constant: Constant): Option[Float] =
          if constant.tag == dotc.core.Constants.FloatTag then Some(constant.floatValue)
          else None
      end Float

      object Double extends DoubleModule:
        def apply(x: Double): Constant = dotc.core.Constants.Constant(x)
        def unapply(constant: Constant): Option[Double] =
          if constant.tag == dotc.core.Constants.DoubleTag then Some(constant.doubleValue)
          else None
      end Double

      object Char extends CharModule:
        def apply(x: Char): Constant = dotc.core.Constants.Constant(x)
        def unapply(constant: Constant): Option[Char] =
          if constant.tag == dotc.core.Constants.CharTag then Some(constant.charValue)
          else None
      end Char

      object String extends StringModule:
        def apply(x: String): Constant = dotc.core.Constants.Constant(x)
        def unapply(constant: Constant): Option[String] =
          if constant.tag == dotc.core.Constants.StringTag then Some(constant.stringValue)
          else None
      end String

      object Unit extends UnitModule:
        def apply(): Constant = dotc.core.Constants.Constant(())
        def unapply(constant: Constant): Boolean =
          constant.tag == dotc.core.Constants.UnitTag
      end Unit

      object Null extends NullModule:
        def apply(): Constant = dotc.core.Constants.Constant(null)
        def unapply(constant: Constant): Boolean =
          constant.tag == dotc.core.Constants.NullTag
      end Null

      object ClassOf extends ClassOfModule:
        def apply(x: TypeRepr): Constant =
          // TODO check that the type is a valid class when creating this constant or let Ycheck do it?
          dotc.core.Constants.Constant(x)
        def unapply(constant: Constant): Option[TypeRepr] =
          if constant.tag == dotc.core.Constants.ClazzTag then Some(constant.typeValue)
          else None
      end ClassOf

    end Constant

    object ConstantMethodsImpl extends ConstantMethods:
      extension (self: Constant):
        def value: Any = self.value
        def showExtractors: String =
          Extractors.showConstant(using QuotesImpl.this)(self)
        def show: String =
          SourceCode.showConstant(using QuotesImpl.this)(self)(SyntaxHighlight.plain, fullNames = true)
        def showShort: String =
          SourceCode.showConstant(using QuotesImpl.this)(self)(SyntaxHighlight.plain, fullNames = false)
        def showAnsiColored: String =
          SourceCode.showConstant(using QuotesImpl.this)(self)(SyntaxHighlight.ANSI, fullNames = true)
      end extension
    end ConstantMethodsImpl

    object Implicits extends ImplicitsModule:
      def search(tpe: TypeRepr): ImplicitSearchResult =
        ctx.typer.inferImplicitArg(tpe, Position.ofMacroExpansion.span)
    end Implicits

    type ImplicitSearchResult = Tree

    type ImplicitSearchSuccess = Tree

    object ImplicitSearchSuccessTypeTestImpl extends TypeTest[ImplicitSearchResult, ImplicitSearchSuccess]:
      def unapply(x: ImplicitSearchResult): Option[ImplicitSearchSuccess & x.type] =
        x.tpe match
          case _: dotc.typer.Implicits.SearchFailureType => None
          case _ => Some(x)
    end ImplicitSearchSuccessTypeTestImpl

    object ImplicitSearchSuccessMethodsImpl extends ImplicitSearchSuccessMethods:
      extension (self: ImplicitSearchSuccess):
        def tree: Term = self
      end extension
    end ImplicitSearchSuccessMethodsImpl

    type ImplicitSearchFailure = Tree

    object ImplicitSearchFailureTypeTestImpl extends TypeTest[ImplicitSearchResult, ImplicitSearchFailure]:
      def unapply(x: ImplicitSearchResult): Option[ImplicitSearchFailure & x.type] =
        x.tpe match
          case _: dotc.typer.Implicits.SearchFailureType => Some(x)
          case _ => None
    end ImplicitSearchFailureTypeTestImpl

    object ImplicitSearchFailureMethodsImpl extends ImplicitSearchFailureMethods:
      extension (self: ImplicitSearchFailure):
        def explanation: String =
          self.tpe.asInstanceOf[dotc.typer.Implicits.SearchFailureType].explanation
      end extension
    end ImplicitSearchFailureMethodsImpl

    type DivergingImplicit = Tree

    object DivergingImplicitTypeTestImpl extends TypeTest[ImplicitSearchResult, DivergingImplicit]:
      def unapply(x: ImplicitSearchResult): Option[DivergingImplicit & x.type] =
        x.tpe match
          case _: dotc.typer.Implicits.DivergingImplicit => Some(x)
          case _ => None
    end DivergingImplicitTypeTestImpl

    type NoMatchingImplicits = Tree

    object NoMatchingImplicitsTypeTestImpl extends TypeTest[ImplicitSearchResult, NoMatchingImplicits]:
      def unapply(x: ImplicitSearchResult): Option[NoMatchingImplicits & x.type] =
        x.tpe match
          case _: dotc.typer.Implicits.NoMatchingImplicits => Some(x)
          case _ => None
    end NoMatchingImplicitsTypeTestImpl

    type AmbiguousImplicits = Tree

    object AmbiguousImplicitsTypeTestImpl extends TypeTest[ImplicitSearchResult, AmbiguousImplicits]:
      def unapply(x: ImplicitSearchResult): Option[AmbiguousImplicits & x.type] =
        x.tpe match
          case _: dotc.typer.Implicits.AmbiguousImplicits => Some(x)
          case _ => None
    end AmbiguousImplicitsTypeTestImpl

    type Symbol = dotc.core.Symbols.Symbol

    object Symbol extends SymbolModule:
      def spliceOwner: Symbol = ctx.owner
      def requiredPackage(path: String): Symbol = dotc.core.Symbols.requiredPackage(path)
      def requiredClass(path: String): Symbol = dotc.core.Symbols.requiredClass(path)
      def requiredModule(path: String): Symbol = dotc.core.Symbols.requiredModule(path)
      def requiredMethod(path: String): Symbol = dotc.core.Symbols.requiredMethod(path)
      def classSymbol(fullName: String): Symbol = dotc.core.Symbols.requiredClass(fullName)
      def newMethod(owner: Symbol, name: String, tpe: TypeRepr): Symbol =
        newMethod(owner, name, tpe, Flags.EmptyFlags, noSymbol)
      def newMethod(owner: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol): Symbol =
        dotc.core.Symbols.newSymbol(owner, name.toTermName, flags | dotc.core.Flags.Method, tpe, privateWithin)
      def newVal(owner: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol): Symbol =
        dotc.core.Symbols.newSymbol(owner, name.toTermName, flags, tpe, privateWithin)
      def newBind(owner: Symbol, name: String, flags: Flags, tpe: TypeRepr): Symbol =
        dotc.core.Symbols.newSymbol(owner, name.toTermName, flags | Case, tpe)
      def noSymbol: Symbol = dotc.core.Symbols.NoSymbol
    end Symbol

    object SymbolMethodsImpl extends SymbolMethods:
      extension (self: Symbol):
        def owner: Symbol = self.denot.owner
        def maybeOwner: Symbol = self.denot.maybeOwner
        def flags: Flags = self.denot.flags

        def privateWithin: Option[TypeRepr] =
          val within: Symbol =
          self.denot.privateWithin
          if (within.exists && !self.is(dotc.core.Flags.Protected)) Some(within.typeRef)
          else None

        def protectedWithin: Option[TypeRepr] =
          val within: Symbol =
          self.denot.privateWithin
          if (within.exists && self.is(dotc.core.Flags.Protected)) Some(within.typeRef)
          else None

        def name: String = self.denot.name.toString
        def fullName: String = self.denot.fullName.toString
        def pos: Position = self.sourcePos

        def documentation: Option[Documentation] =
          import dotc.core.Comments.CommentsContext
          val docCtx = ctx.docCtx.getOrElse {
            throw new RuntimeException(
              "DocCtx could not be found and documentations are unavailable. This is a compiler-internal error."
            )
          }
          docCtx.docstring(self)

        def tree: Tree = FromSymbol.definitionFromSym(self)

        def annots: List[Term] =
          self.annotations.flatMap {
            case _: dotc.core.Annotations.BodyAnnotation => Nil
            case annot => annot.tree :: Nil
          }

        def isDefinedInCurrentRun: Boolean =
          self.topLevelClass.asClass.isDefinedInCurrentRun
        def isLocalDummy: Boolean = self.denot.isLocalDummy
        def isRefinementClass: Boolean = self.denot.isRefinementClass
        def isAliasType: Boolean = self.denot.isAliasType
        def isAnonymousClass: Boolean = self.denot.isAnonymousClass
        def isAnonymousFunction: Boolean = self.denot.isAnonymousFunction
        def isAbstractType: Boolean = self.denot.isAbstractType
        def isClassConstructor: Boolean = self.denot.isClassConstructor
        def isType: Boolean = self.isType
        def isTerm: Boolean = self.isTerm
        def isPackageDef: Boolean = self.is(dotc.core.Flags.Package)
        def isClassDef: Boolean = self.isClass
        def isTypeDef: Boolean =
          self.isType && !self.isClass && !self.is(dotc.core.Flags.Case)
        def isValDef: Boolean =
          self.isTerm && !self.is(dotc.core.Flags.Method) && !self.is(dotc.core.Flags.Case/*, FIXME add this check and fix sourcecode butNot = Enum | Module*/)
        def isDefDef: Boolean = self.is(dotc.core.Flags.Method)
        def isBind: Boolean =
          self.is(dotc.core.Flags.Case, butNot = Enum | Module) && !self.isClass
        def isNoSymbol: Boolean = self == Symbol.noSymbol
        def exists: Boolean = self != Symbol.noSymbol
        def fields: List[Symbol] = self.unforcedDecls.filter(isField)

        def field(name: String): Symbol =
          val sym = self.unforcedDecls.find(sym => sym.name == name.toTermName)
          if (isField(sym)) sym else dotc.core.Symbols.NoSymbol

        def classMethod(name: String): List[Symbol] =
          self.typeRef.decls.iterator.collect {
            case sym if isMethod(sym) && sym.name.toString == name => sym.asTerm
          }.toList

        def classMethods: List[Symbol] =
          self.typeRef.decls.iterator.collect {
            case sym if isMethod(sym) => sym.asTerm
          }.toList

        def members: List[Symbol] =
          self.typeRef.info.decls.toList

        def typeMembers: List[Symbol] =
          self.unforcedDecls.filter(_.isType)

        def typeMember(name: String): Symbol =
          self.unforcedDecls.find(sym => sym.name == name.toTypeName)

        def method(name: String): List[Symbol] =
          appliedTypeRef(self).allMembers.iterator.map(_.symbol).collect {
            case sym if isMethod(sym) && sym.name.toString == name => sym.asTerm
          }.toList

        def methods: List[Symbol] =
          appliedTypeRef(self).allMembers.iterator.map(_.symbol).collect {
            case sym if isMethod(sym) => sym.asTerm
          }.toList

        def paramSymss: List[List[Symbol]] = self.denot.paramSymss
        def primaryConstructor: Symbol = self.denot.primaryConstructor
        def allOverriddenSymbols: Iterator[Symbol] = self.denot.allOverriddenSymbols
        def overridingSymbol(ofclazz: Symbol): Symbol =
          if ofclazz.isClass then self.denot.overridingSymbol(ofclazz.asClass)
          else dotc.core.Symbols.NoSymbol

        def caseFields: List[Symbol] =
          if !self.isClass then Nil
          else self.asClass.paramAccessors.collect {
            case sym if sym.is(dotc.core.Flags.CaseAccessor) => sym.asTerm
          }

        def isTypeParam: Boolean = self.isTypeParam
        def signature: Signature = self.signature
        def moduleClass: Symbol = self.denot.moduleClass
        def companionClass: Symbol = self.denot.companionClass
        def companionModule: Symbol = self.denot.companionModule
        def children: List[Symbol] = self.denot.children

        def showExtractors: String =
          Extractors.showSymbol(using QuotesImpl.this)(self)
        def show: String =
          SourceCode.showSymbol(using QuotesImpl.this)(self)(SyntaxHighlight.plain)
        def showAnsiColored: String =
          SourceCode.showSymbol(using QuotesImpl.this)(self)(SyntaxHighlight.ANSI)

      end extension

      private def appliedTypeRef(sym: Symbol): TypeRepr =
        sym.typeRef.appliedTo(sym.typeParams.map(_.typeRef))

      private def isMethod(sym: Symbol): Boolean =
        sym.isTerm && sym.is(dotc.core.Flags.Method) && !sym.isConstructor

      private def isField(sym: Symbol): Boolean =
        sym.isTerm && !sym.is(dotc.core.Flags.Method)
    end SymbolMethodsImpl

    type Signature = dotc.core.Signature

    object Signature extends SignatureModule:
      def unapply(sig: Signature): Option[(List[String | Int], String)] =
        Some((sig.paramSigs, sig.resultSig))
    end Signature

    object SignatureMethodsImpl extends SignatureMethods:
      extension (self: Signature):
        def paramSigs: List[String | Int] =
          self.paramsSig.map {
            case paramSig: dotc.core.Names.TypeName =>
              paramSig.toString
            case paramSig: Int =>
              paramSig
          }
        def resultSig: String =
          self.resSig.toString
      end extension
    end SignatureMethodsImpl

    object defn extends defnModule:
      def RootPackage: Symbol = dotc.core.Symbols.defn.RootPackage
      def RootClass: Symbol = dotc.core.Symbols.defn.RootClass
      def EmptyPackageClass: Symbol = dotc.core.Symbols.defn.EmptyPackageClass
      def ScalaPackage: Symbol = dotc.core.Symbols.defn.ScalaPackageVal
      def ScalaPackageClass: Symbol = dotc.core.Symbols.defn.ScalaPackageClass
      def AnyClass: Symbol = dotc.core.Symbols.defn.AnyClass
      def AnyValClass: Symbol = dotc.core.Symbols.defn.AnyValClass
      def ObjectClass: Symbol = dotc.core.Symbols.defn.ObjectClass
      def AnyRefClass: Symbol = dotc.core.Symbols.defn.AnyRefAlias
      def NullClass: Symbol = dotc.core.Symbols.defn.NullClass
      def NothingClass: Symbol = dotc.core.Symbols.defn.NothingClass
      def UnitClass: Symbol = dotc.core.Symbols.defn.UnitClass
      def ByteClass: Symbol = dotc.core.Symbols.defn.ByteClass
      def ShortClass: Symbol = dotc.core.Symbols.defn.ShortClass
      def CharClass: Symbol = dotc.core.Symbols.defn.CharClass
      def IntClass: Symbol = dotc.core.Symbols.defn.IntClass
      def LongClass: Symbol = dotc.core.Symbols.defn.LongClass
      def FloatClass: Symbol = dotc.core.Symbols.defn.FloatClass
      def DoubleClass: Symbol = dotc.core.Symbols.defn.DoubleClass
      def BooleanClass: Symbol = dotc.core.Symbols.defn.BooleanClass
      def StringClass: Symbol = dotc.core.Symbols.defn.StringClass
      def ClassClass: Symbol = dotc.core.Symbols.defn.ClassClass
      def ArrayClass: Symbol = dotc.core.Symbols.defn.ArrayClass
      def PredefModule: Symbol = dotc.core.Symbols.defn.ScalaPredefModule
      def Predef_classOf: Symbol = dotc.core.Symbols.defn.Predef_classOf
      def JavaLangPackage: Symbol = dotc.core.Symbols.defn.JavaLangPackageVal
      def ArrayModule: Symbol = dotc.core.Symbols.defn.ArrayModule
      def Array_apply: Symbol = dotc.core.Symbols.defn.Array_apply
      def Array_clone: Symbol = dotc.core.Symbols.defn.Array_clone
      def Array_length: Symbol = dotc.core.Symbols.defn.Array_length
      def Array_update: Symbol = dotc.core.Symbols.defn.Array_update
      def RepeatedParamClass: Symbol = dotc.core.Symbols.defn.RepeatedParamClass
      def RepeatedAnnot: Symbol = dotc.core.Symbols.defn.RepeatedAnnot
      def OptionClass: Symbol = dotc.core.Symbols.defn.OptionClass
      def NoneModule: Symbol = dotc.core.Symbols.defn.NoneModule
      def SomeModule: Symbol = dotc.core.Symbols.defn.SomeClass.companionModule
      def ProductClass: Symbol = dotc.core.Symbols.defn.ProductClass
      def FunctionClass(arity: Int, isImplicit: Boolean = false, isErased: Boolean = false): Symbol =
        dotc.core.Symbols.defn.FunctionClass(arity, isImplicit, isErased)
      def TupleClass(arity: Int): Symbol =
        dotc.core.Symbols.defn.TupleType(arity).classSymbol.asClass
      def isTupleClass(sym: Symbol): Boolean =
        dotc.core.Symbols.defn.isTupleClass(sym)
      def ScalaPrimitiveValueClasses: List[Symbol] =
        UnitClass :: BooleanClass :: ScalaNumericValueClasses
      def ScalaNumericValueClasses: List[Symbol] =
        ByteClass :: ShortClass :: IntClass :: LongClass :: FloatClass :: DoubleClass :: CharClass :: Nil
    end defn

    type Flags = dotc.core.Flags.FlagSet

    object Flags extends FlagsModule:
      def Abstract: Flags = dotc.core.Flags.Abstract
      def Artifact: Flags = dotc.core.Flags.Artifact
      def Case: Flags = dotc.core.Flags.Case
      def CaseAccessor: Flags = dotc.core.Flags.CaseAccessor
      def Contravariant: Flags = dotc.core.Flags.Contravariant
      def Covariant: Flags = dotc.core.Flags.Covariant
      def EmptyFlags = dotc.core.Flags.EmptyFlags
      def Enum: Flags = dotc.core.Flags.Enum
      def Erased: Flags = dotc.core.Flags.Erased
      def Exported: Flags = dotc.core.Flags.Exported
      def ExtensionMethod: Flags = dotc.core.Flags.ExtensionMethod
      def FieldAccessor: Flags = dotc.core.Flags.Accessor
      def Final: Flags = dotc.core.Flags.Final
      def Given: Flags = dotc.core.Flags.Given
      def HasDefault: Flags = dotc.core.Flags.HasDefault
      def Implicit: Flags = dotc.core.Flags.Implicit
      def Inline: Flags = dotc.core.Flags.Inline
      def JavaDefined: Flags = dotc.core.Flags.JavaDefined
      def Lazy: Flags = dotc.core.Flags.Lazy
      def Local: Flags = dotc.core.Flags.Local
      def Macro: Flags = dotc.core.Flags.Macro
      def ModuleClass: Flags = dotc.core.Flags.ModuleClass
      def Mutable: Flags = dotc.core.Flags.Mutable
      def Object: Flags = dotc.core.Flags.Module
      def Opaque: Flags = dotc.core.Flags.Opaque
      def Open: Flags = dotc.core.Flags.Open
      def Override: Flags = dotc.core.Flags.Override
      def Package: Flags = dotc.core.Flags.Package
      def Param: Flags = dotc.core.Flags.Param
      def ParamAccessor: Flags = dotc.core.Flags.ParamAccessor
      def Private: Flags = dotc.core.Flags.Private
      def PrivateLocal: Flags = dotc.core.Flags.PrivateLocal
      def Protected: Flags = dotc.core.Flags.Protected
      def Scala2x: Flags = dotc.core.Flags.Scala2x
      def Sealed: Flags = dotc.core.Flags.Sealed
      def StableRealizable: Flags = dotc.core.Flags.StableRealizable
      def Static: Flags = dotc.core.Flags.JavaStatic
      def Synthetic: Flags = dotc.core.Flags.Synthetic
      def Trait: Flags = dotc.core.Flags.Trait
    end Flags

    object FlagsMethodsImpl extends FlagsMethods:
      extension (self: Flags):
        def is(that: Flags): Boolean = self.isAllOf(that)
        def |(that: Flags): Flags = dotc.core.Flags.or(self, that) // TODO: Replace with dotc.core.Flags.|(self)(that)  once extension names have stabilized
        def &(that: Flags): Flags = dotc.core.Flags.and(self, that)// TODO: Replace with dotc.core.Flags.&(self)(that)  once extension names have stabilized
        def showExtractors: String =
          Extractors.showFlags(using QuotesImpl.this)(self)
        def show: String =
          SourceCode.showFlags(using QuotesImpl.this)(self)(SyntaxHighlight.plain)
        def showAnsiColored: String =
          SourceCode.showFlags(using QuotesImpl.this)(self)(SyntaxHighlight.ANSI)
      end extension
    end FlagsMethodsImpl

    type Position = dotc.util.SourcePosition

    object Position extends PositionModule:
      def ofMacroExpansion: dotc.util.SourcePosition =
        MacroExpansion.position.getOrElse(dotc.util.SourcePosition(ctx.source, dotc.util.Spans.NoSpan))
    end Position

    object PositionMethodsImpl extends PositionMethods:
      extension (self: Position):
        def start: Int = self.start
        def end: Int = self.end
        def exists: Boolean = self.exists
        def sourceFile: SourceFile = self.source
        def startLine: Int = self.startLine
        def endLine: Int = self.endLine
        def startColumn: Int = self.startColumn
        def endColumn: Int = self.endColumn
        def sourceCode: String =
          new String(self.source.content(), self.start, self.end - self.start)
      end extension
    end PositionMethodsImpl

    type SourceFile = dotc.util.SourceFile

    object SourceFile extends SourceFileModule

    object SourceFileMethodsImpl extends SourceFileMethods:
      extension (self: SourceFile):
        def jpath: java.nio.file.Path = self.file.jpath
        def content: String = new String(self.content())
      end extension
    end SourceFileMethodsImpl

    object Source extends SourceModule:
      def path: java.nio.file.Path = ctx.compilationUnit.source.file.jpath
    end Source

    object report extends reportModule:

      def error(msg: String): Unit =
        dotc.report.error(msg, Position.ofMacroExpansion)

      def error(msg: String, expr: Expr[Any]): Unit =
        dotc.report.error(msg, Term.of(expr).pos)

      def error(msg: String, pos: Position): Unit =
        dotc.report.error(msg, pos)

      def error(msg: String, sourceFile: SourceFile, start: Int, end: Int): Unit =
        dotc.report.error(msg, dotc.util.SourcePosition(sourceFile, dotc.util.Spans.Span(start, end)))

      def throwError(msg: String): Nothing =
        error(msg)
        throw new scala.quoted.runtime.StopMacroExpansion

      def throwError(msg: String, expr: Expr[Any]): Nothing =
        error(msg, expr)
        throw new scala.quoted.runtime.StopMacroExpansion

      def throwError(msg: String, pos: Position): Nothing =
        error(msg, pos)
        throw new scala.quoted.runtime.StopMacroExpansion

      def throwError(msg: String, sourceFile: SourceFile, start: Int, end: Int): Nothing =
        error(msg, sourceFile, start, end)
        throw new scala.quoted.runtime.StopMacroExpansion

      def warning(msg: String): Unit =
        dotc.report.warning(msg, Position.ofMacroExpansion)

      def warning(msg: String, expr: Expr[Any]): Unit =
        dotc.report.warning(msg, Term.of(expr).pos)

      def warning(msg: String, pos: Position): Unit =
        dotc.report.warning(msg, pos)

      def warning(msg: String, sourceFile: SourceFile, start: Int, end: Int): Unit =
        dotc.report.error(msg, dotc.util.SourcePosition(sourceFile, dotc.util.Spans.Span(start, end)))

    end report

    type Documentation = dotc.core.Comments.Comment

    object Documentation extends DocumentationModule

    object DocumentationMethodsImpl extends DocumentationMethods:
      extension (self: Documentation):
        def raw: String = self.raw
        def expanded: Option[String] = self.expanded
        def usecases: List[(String, Option[DefDef])] =
          self.usecases.map { uc => (uc.code, uc.tpdCode) }
      end extension
    end DocumentationMethodsImpl

    private def optional[T <: dotc.ast.Trees.Tree[?]](tree: T): Option[tree.type] =
      if tree.isEmpty then None else Some(tree)

    private def withDefaultPos[T <: Tree](fn: Context ?=> T): T =
      fn(using ctx.withSource(Position.ofMacroExpansion.source)).withSpan(Position.ofMacroExpansion.span)

    /** Checks that all definitions in this tree have the expected owner.
     *  Nested definitions are ignored and assumed to be correct by construction.
     */
    private def yCheckedOwners(tree: Option[Tree], owner: Symbol): tree.type =
      if yCheck then
        tree match
          case Some(tree) =>
            yCheckOwners(tree, owner)
          case _ =>
      tree

    /** Checks that all definitions in this tree have the expected owner.
     *  Nested definitions are ignored and assumed to be correct by construction.
     */
    private def yCheckedOwners(tree: Tree, owner: Symbol): tree.type =
      if yCheck then
        yCheckOwners(tree, owner)
      tree

    /** Checks that all definitions in this tree have the expected owner.
     *  Nested definitions are ignored and assumed to be correct by construction.
     */
    private def yCheckOwners(tree: Tree, owner: Symbol): Unit =
      new tpd.TreeTraverser {
        def traverse(t: Tree)(using Context): Unit =
          t match
            case t: tpd.DefTree =>
              val defOwner = t.symbol.owner
              assert(defOwner == owner,
                s"""Tree had an unexpected owner for ${t.symbol}
                   |Expected: $owner (${owner.fullName})
                   |But was: $defOwner (${defOwner.fullName})
                   |
                   |
                   |The code of the definition of ${t.symbol} is
                   |${TreeMethods.show(t)}
                   |
                   |which was found in the code
                   |${TreeMethods.show(tree)}
                   |
                   |which has the AST representation
                   |${TreeMethods.showExtractors(tree)}
                   |
                   |""".stripMargin)
            case _ => traverseChildren(t)
      }.traverse(tree)

  end reflect

  def unpickleExpr[T](pickled: String | List[String], typeHole: (Int, Seq[Any]) => scala.quoted.Type[?], termHole: (Int, Seq[Any], scala.quoted.Quotes) => scala.quoted.Expr[?]): scala.quoted.Expr[T] =
    val tree = PickledQuotes.unpickleTerm(pickled, typeHole, termHole)
    new ExprImpl(tree, hash).asInstanceOf[scala.quoted.Expr[T]]

  def unpickleType[T <: AnyKind](pickled: String | List[String], typeHole: (Int, Seq[Any]) => scala.quoted.Type[?], termHole: (Int, Seq[Any], scala.quoted.Quotes) => scala.quoted.Expr[?]): scala.quoted.Type[T] =
    val tree = PickledQuotes.unpickleTypeTree(pickled, typeHole, termHole)
    new TypeImpl(tree, hash).asInstanceOf[scala.quoted.Type[T]]

  object ExprMatch extends ExprMatchModule:
    def unapply[TypeBindings <: Tuple, Tup <: Tuple](scrutinee: scala.quoted.Expr[Any])(using pattern: scala.quoted.Expr[Any]): Option[Tup] =
      val scrutineeTree = reflect.Term.of(scrutinee)
      val patternTree = reflect.Term.of(pattern)
      treeMatch(scrutineeTree, patternTree).asInstanceOf[Option[Tup]]
  end ExprMatch

  object TypeMatch extends TypeMatchModule:
    def unapply[TypeBindings <: Tuple, Tup <: Tuple](scrutinee: scala.quoted.Type[?])(using pattern: scala.quoted.Type[?]): Option[Tup] =
      val scrutineeTree = reflect.TypeTree.of(using scrutinee)
      val patternTree = reflect.TypeTree.of(using pattern)
      treeMatch(scrutineeTree, patternTree).asInstanceOf[Option[Tup]]
  end TypeMatch

  private def treeMatch(scrutinee: reflect.Tree, pattern: reflect.Tree): Option[Tuple] = {
    import reflect._
    def isTypeHoleDef(tree: Tree): Boolean =
      tree match
        case tree: TypeDef =>
          tree.symbol.hasAnnotation(dotc.core.Symbols.defn.QuotedRuntimePatterns_patternTypeAnnot)
        case _ => false

    def extractTypeHoles(pat: Term): (Term, List[Symbol]) =
      pat match
        case tpd.Inlined(_, Nil, pat2) => extractTypeHoles(pat2)
        case tpd.Block(stats @ ((typeHole: TypeDef) :: _), expr) if isTypeHoleDef(typeHole) =>
          val holes = stats.takeWhile(isTypeHoleDef).map(_.symbol)
          val otherStats = stats.dropWhile(isTypeHoleDef)
          (tpd.cpy.Block(pat)(otherStats, expr), holes)
        case _ =>
          (pat, Nil)

    val (pat1, typeHoles) = extractTypeHoles(pattern)

    val ctx1 =
      if typeHoles.isEmpty then ctx
      else
        val ctx1 = ctx.fresh.setFreshGADTBounds.addMode(dotc.core.Mode.GadtConstraintInference)
        ctx1.gadt.addToConstraint(typeHoles)
        ctx1

    val qctx1 = QuotesImpl()(using ctx1)

    val matcher = new Matcher.QuoteMatcher[qctx1.type](qctx1) {
      def patternHoleSymbol: qctx1.reflect.Symbol = dotc.core.Symbols.defn.QuotedRuntimePatterns_patternHole.asInstanceOf
      def higherOrderHoleSymbol: qctx1.reflect.Symbol = dotc.core.Symbols.defn.QuotedRuntimePatterns_higherOrderHole.asInstanceOf
    }

    val matchings =
      if pat1.isType then matcher.termMatch(scrutinee.asInstanceOf[matcher.qctx.reflect.Term], pat1.asInstanceOf[matcher.qctx.reflect.Term])
      else matcher.termMatch(scrutinee.asInstanceOf[matcher.qctx.reflect.Term], pat1.asInstanceOf[matcher.qctx.reflect.Term])

    // val matchings = matcher.termMatch(scrutinee, pattern)
    if typeHoles.isEmpty then matchings
    else {
      // After matching and doing all subtype checks, we have to approximate all the type bindings
      // that we have found, seal them in a quoted.Type and add them to the result
      def typeHoleApproximation(sym: Symbol) =
        ctx1.gadt.approximation(sym, !sym.hasAnnotation(dotc.core.Symbols.defn.QuotedRuntimePatterns_fromAboveAnnot)).asInstanceOf[qctx1.reflect.TypeRepr].asType
      matchings.map { tup =>
        Tuple.fromIArray(typeHoles.map(typeHoleApproximation).toArray.asInstanceOf[IArray[Object]]) ++ tup
      }
    }
  }

  private[this] val hash = QuotesImpl.scopeId(using ctx)
  override def hashCode: Int = hash

end QuotesImpl
