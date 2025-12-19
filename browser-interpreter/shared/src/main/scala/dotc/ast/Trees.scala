package dotc.ast

import dotc.core.Names._
import dotc.core.Constants.Constant
import dotc.core.Flags._
import dotc.util.{SourceFile, Span}

/**
 * Cross-platform AST trees for the browser compiler.
 *
 * This is a simplified tree representation for parsing Scala source code.
 */
object Trees {

  /** Marker for untyped trees */
  type Untyped = Null

  /** Base class for all tree nodes */
  abstract class Tree {
    /** Source position span */
    var span: Span = Span.NoSpan

    /** Is this a term? */
    def isTerm: Boolean = false

    /** Is this a type? */
    def isType: Boolean = false

    /** Is this a pattern? */
    def isPattern: Boolean = false

    /** Is this empty? */
    def isEmpty: Boolean = false

    /** Set span and return this */
    def withSpan(span: Span): this.type = {
      this.span = span
      this
    }

    /** For debugging */
    def show: String = this.toString
  }

  /** A tree with a name */
  trait NameTree extends Tree {
    def name: Name
  }

  /** A tree representing a definition */
  trait DefTree extends Tree

  /** A tree representing a term */
  trait TermTree extends Tree {
    override def isTerm: Boolean = true
  }

  /** A tree representing a type */
  trait TypTree extends Tree {
    override def isType: Boolean = true
  }

  /** A tree representing a pattern */
  trait PatternTree extends Tree {
    override def isPattern: Boolean = true
  }

  // ============= Identifiers and References =============

  /** An identifier */
  case class Ident(name: Name) extends Tree with NameTree {
    override def isTerm: Boolean = name.isTermName
    override def isType: Boolean = name.isTypeName
  }

  /** A selection qual.name */
  case class Select(qualifier: Tree, name: Name) extends Tree with NameTree {
    override def isTerm: Boolean = name.isTermName
    override def isType: Boolean = name.isTypeName
  }

  /** this */
  case class This(qual: TypeName) extends Tree with TermTree

  /** super */
  case class Super(qual: Tree, mix: TypeName) extends Tree with TermTree

  // ============= Literals =============

  /** A literal value */
  case class Literal(const: Constant) extends Tree with TermTree

  // ============= Expressions =============

  /** Function application f(args) */
  case class Apply(fun: Tree, args: List[Tree]) extends Tree with TermTree

  /** Type application f[targs] */
  case class TypeApply(fun: Tree, args: List[Tree]) extends Tree with TermTree

  /** new T(args) */
  case class New(tpt: Tree) extends Tree with TermTree

  /** (expr: tpt) */
  case class Typed(expr: Tree, tpt: Tree) extends Tree with TermTree

  /** name = value */
  case class Assign(lhs: Tree, rhs: Tree) extends Tree with TermTree

  /** { stats; expr } */
  case class Block(stats: List[Tree], expr: Tree) extends Tree with TermTree

  /** if (cond) thenp else elsep */
  case class If(cond: Tree, thenp: Tree, elsep: Tree) extends Tree with TermTree

  /** expr match { cases } */
  case class Match(selector: Tree, cases: List[CaseDef]) extends Tree with TermTree

  /** case pat if guard => body */
  case class CaseDef(pat: Tree, guard: Tree, body: Tree) extends Tree

  /** try block catch { cases } finally finalizer */
  case class Try(expr: Tree, cases: List[CaseDef], finalizer: Tree) extends Tree with TermTree

  /** throw expr */
  case class Throw(expr: Tree) extends Tree with TermTree

  /** return expr */
  case class Return(expr: Tree, from: Tree) extends Tree with TermTree

  /** while (cond) body */
  case class WhileDo(cond: Tree, body: Tree) extends Tree with TermTree

  /** (t1, ..., tn) or (t) */
  case class Tuple(trees: List[Tree]) extends Tree {
    override def isTerm: Boolean = trees.isEmpty || trees.head.isTerm
    override def isType: Boolean = !isTerm
  }

  /** A named argument name = arg */
  case class NamedArg(name: Name, arg: Tree) extends Tree with TermTree

  /** A sequence of arguments (vararg) */
  case class SeqLiteral(elems: List[Tree], elemtpt: Tree) extends Tree with TermTree

  /** An inlined call */
  case class Inlined(call: Tree, bindings: List[Tree], expansion: Tree) extends Tree with TermTree

  // ============= Lambdas =============

  /** (params) => body */
  case class Function(args: List[Tree], body: Tree) extends Tree {
    override def isTerm: Boolean = body.isTerm
    override def isType: Boolean = body.isType
  }

  /** { cases } as partial function */
  case class Closure(env: List[Tree], meth: Tree, tpt: Tree) extends Tree with TermTree

  // ============= Patterns =============

  /** pattern @ pattern */
  case class Bind(name: Name, body: Tree) extends Tree with DefTree with PatternTree

  /** pat1 | pat2 */
  case class Alternative(trees: List[Tree]) extends Tree with PatternTree

  /** Extractor(patterns) */
  case class UnApply(fun: Tree, implicits: List[Tree], patterns: List[Tree]) extends Tree with PatternTree

  // ============= Type Trees =============

  /** Applied type T[args] */
  case class AppliedTypeTree(tpt: Tree, args: List[Tree]) extends Tree with TypTree

  /** Type bounds >: lo <: hi */
  case class TypeBoundsTree(lo: Tree, hi: Tree) extends Tree with TypTree

  /** Refined type tpt { refinements } */
  case class RefinedTypeTree(tpt: Tree, refinements: List[Tree]) extends Tree with TypTree

  /** By-name type => T */
  case class ByNameTypeTree(result: Tree) extends Tree with TypTree

  /** Match type tpt match { cases } */
  case class MatchTypeTree(bound: Tree, selector: Tree, cases: List[CaseDef]) extends Tree with TypTree

  /** Annotated type tpt @annot */
  case class Annotated(arg: Tree, annot: Tree) extends Tree {
    override def isTerm: Boolean = arg.isTerm
    override def isType: Boolean = arg.isType
  }

  /** Type lambda [X] =>> T */
  case class LambdaTypeTree(tparams: List[TypeDef], body: Tree) extends Tree with TypTree

  /** Singleton type x.type */
  case class SingletonTypeTree(ref: Tree) extends Tree with TypTree

  // ============= Definitions =============

  /** Modifiers for definitions */
  case class Modifiers(
    flags: FlagSet = EmptyFlags,
    privateWithin: TypeName = null,
    annotations: List[Tree] = Nil
  ) {
    def is(flag: Flag): Boolean = flags.is(flag)
    def isOneOf(fs: FlagSet): Boolean = flags.isOneOf(fs)
    def | (flag: Flag): Modifiers = Modifiers(flags | flag, privateWithin, annotations)
    def withAnnotations(annots: List[Tree]): Modifiers = Modifiers(flags, privateWithin, annotations ++ annots)
    def withPrivateWithin(within: TypeName): Modifiers = Modifiers(flags, within, annotations)
  }

  object Modifiers {
    val Empty: Modifiers = Modifiers()
  }

  /** val/var name: tpt = rhs */
  case class ValDef(name: TermName, tpt: Tree, rhs: Tree) extends Tree with DefTree with NameTree {
    var mods: Modifiers = Modifiers.Empty
    def withMods(mods: Modifiers): ValDef = { this.mods = mods; this }
  }

  /** def name[tparams](params): tpt = rhs */
  case class DefDef(name: TermName, paramss: List[ParamClause], tpt: Tree, rhs: Tree) extends Tree with DefTree with NameTree {
    var mods: Modifiers = Modifiers.Empty
    def withMods(mods: Modifiers): DefDef = { this.mods = mods; this }
  }

  /** type name[tparams] = rhs or type name[tparams] >: lo <: hi */
  case class TypeDef(name: TypeName, rhs: Tree) extends Tree with DefTree with NameTree {
    var mods: Modifiers = Modifiers.Empty
    def withMods(mods: Modifiers): TypeDef = { this.mods = mods; this }
  }

  /** class/trait/object name[tparams](params) extends template */
  case class ClassDef(name: TypeName, tparams: List[TypeDef], template: Template) extends Tree with DefTree with NameTree {
    var mods: Modifiers = Modifiers.Empty
    def withMods(mods: Modifiers): ClassDef = { this.mods = mods; this }
  }

  /** A parameter clause */
  sealed trait ParamClause
  case class TermParamClause(params: List[ValDef]) extends ParamClause
  case class TypeParamClause(params: List[TypeDef]) extends ParamClause

  /** Template: extends parents { self => body } */
  case class Template(constr: DefDef, parents: List[Tree], self: ValDef, body: List[Tree]) extends Tree with DefTree

  /** package name { stats } */
  case class PackageDef(pid: Tree, stats: List[Tree]) extends Tree with DefTree

  /** import qual.selectors */
  case class Import(expr: Tree, selectors: List[ImportSelector]) extends Tree with DefTree

  /** Export qual.selectors */
  case class Export(expr: Tree, selectors: List[ImportSelector]) extends Tree with DefTree

  /** Import selector: name, name => rename, name => _ */
  case class ImportSelector(imported: Ident, renamed: Tree, bound: Tree) extends Tree

  // ============= Annotations =============

  /** @annot or @annot(args) */
  case class Annotation(tree: Tree) extends Tree

  // ============= Empty/Thicket =============

  /** Empty tree */
  case object EmptyTree extends Tree {
    override def isEmpty: Boolean = true
  }

  /** Multiple trees that count as one */
  case class Thicket(trees: List[Tree]) extends Tree {
    override def isEmpty: Boolean = trees.isEmpty
  }

  // ============= Untyped-only trees =============

  /** Infix operation: left op right */
  case class InfixOp(left: Tree, op: Ident, right: Tree) extends Tree with TermTree

  /** Postfix operation: od op */
  case class PostfixOp(od: Tree, op: Ident) extends Tree with TermTree

  /** Prefix operation: op od */
  case class PrefixOp(op: Ident, od: Tree) extends Tree with TermTree

  /** Parenthesized expression */
  case class Parens(t: Tree) extends Tree {
    override def isTerm: Boolean = t.isTerm
    override def isType: Boolean = t.isType
  }

  /** for (enums) yield expr */
  case class ForYield(enums: List[Tree], expr: Tree) extends Tree with TermTree

  /** for (enums) do body */
  case class ForDo(enums: List[Tree], body: Tree) extends Tree with TermTree

  /** Generator: pat <- expr */
  case class GenFrom(pat: Tree, expr: Tree) extends Tree

  /** Value definition in for: pat = expr */
  case class GenAlias(pat: Tree, expr: Tree) extends Tree

  /** Interpolated string */
  case class InterpolatedString(id: TermName, segments: List[Tree]) extends Tree with TermTree

  /** Pattern definition: val x, y = rhs */
  case class PatDef(pats: List[Tree], tpt: Tree, rhs: Tree) extends Tree with DefTree {
    var mods: Modifiers = Modifiers.Empty
    def withMods(mods: Modifiers): PatDef = { this.mods = mods; this }
  }

  /** Module (object) definition */
  case class ModuleDef(name: TermName, impl: Template) extends Tree with DefTree with NameTree {
    var mods: Modifiers = Modifiers.Empty
    def withMods(mods: Modifiers): ModuleDef = { this.mods = mods; this }
  }
}

/** Untyped trees */
object untpd {
  export Trees._

  /** Create an empty tree */
  def emptyTree: Tree = EmptyTree

  /** Create an identifier from a string */
  def Ident(name: String): Trees.Ident = Trees.Ident(termName(name))

  /** Create a type identifier */
  def TypeIdent(name: String): Trees.Ident = Trees.Ident(typeName(name))
}

