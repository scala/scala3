package dotty.tools
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, NameOps._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._
import TreeInfo._
import Decorators._
import language.higherKinds
import collection.mutable.ListBuffer

object untpd extends Trees.Instance[Untyped] {

  val EmptyTree = emptyTree[Untyped]()

// ----- Tree cases that exist in untyped form only ------------------

  /** A typed subtree of an untyped tree needs to be wrapped in a TypedSlice */
  case class TypedSplice(tree: tpd.Tree) extends Tree

  /** mods object name impl */
  case class ModuleDef(mods: Modifiers, name: TermName, impl: Template)
    extends MemberDef {
    type ThisTree[T >: Untyped] <: Trees.NameTree[T] with Trees.MemberDef[T] with ModuleDef
    def withName(name: Name) = this.derivedModuleDef(mods, name.toTermName, impl)
  }

  case class SymbolLit(str: String) extends Tree
  case class InterpolatedString(id: TermName, strings: List[Literal], elems: List[Tree]) extends Tree
  case class Function(args: List[Tree], body: Tree) extends Tree
  case class InfixOp(left: Tree, op: Name, right: Tree) extends Tree
  case class PostfixOp(od: Tree, op: Name) extends Tree
  case class PrefixOp(op: Name, od: Tree) extends Tree
  case class Parens(t: Tree) extends Tree
  case class Tuple(trees: List[Tree]) extends Tree
  case class WhileDo(cond: Tree, body: Tree) extends TermTree
  case class DoWhile(body: Tree, cond: Tree) extends TermTree
  case class ForYield(enums: List[Tree], expr: Tree) extends TermTree
  case class ForDo(enums: List[Tree], body: Tree) extends TermTree
  case class GenFrom(pat: Tree, expr: Tree) extends Tree
  case class GenAlias(pat: Tree, expr: Tree) extends Tree
  case class ContextBounds(bounds: TypeBoundsTree, cxBounds: List[Tree]) extends TypTree
  case class PatDef(mods: Modifiers, pats: List[Tree], tpt: Tree, rhs: Tree) extends Tree

// ------ Untyped tree values and creation methods ---------------------

  val unitLiteral = Literal(Constant())

  def ref(tp: NamedType)(implicit ctx: Context): Tree =
    TypedSplice(tpd.ref(tp))

  def scalaUnit(implicit ctx: Context) = ref(defn.UnitClass.typeConstructor)

  def makeConstructor(mods: Modifiers, tparams: List[TypeDef], vparamss: List[List[ValDef]], rhs: Tree = EmptyTree): DefDef =
    DefDef(mods, nme.CONSTRUCTOR, tparams, vparamss, TypeTree(), rhs)

  def emptyConstructor: DefDef =
    makeConstructor(Modifiers(), Nil, Nil)

  def makeSelfDef(name: TermName, tpt: Tree)(implicit ctx: Context) =
    ValDef(Modifiers(Private), name, tpt, EmptyTree)

  def makeTupleOrParens(ts: List[Tree])(implicit ctx: Context) = ts match {
    case t :: Nil => Parens(t)
    case _ => Tuple(ts)
  }

  def makeTuple(ts: List[Tree])(implicit ctx: Context) = ts match {
    case t :: Nil => t
    case _ => Tuple(ts)
  }

  def makeParameter(pname: TermName, tpe: Tree, mods: Modifiers = Modifiers()): ValDef =
    ValDef(mods | Param, pname, tpe, emptyTree())

  def makeSyntheticParameter(n: Int = 1, tpt: Tree = EmptyTree)(implicit ctx: Context): ValDef =
    ValDef(Modifiers(SyntheticTermParam), nme.syntheticParamName(n), TypeTree(), EmptyTree)

  def refOfDef(tree: NameTree) = Ident(tree.name)

// ------- A decorator for producing a path to a location --------------

  implicit class UntypedTreeDecorator(val self: Tree) extends AnyVal {
    def locateEnclosing(base: List[Tree], pos: Position): List[Tree] = {
      def encloses(elem: Any) = elem match {
        case t: Tree => t.envelope contains pos
        case _ => false
      }
      base.productIterator find encloses match {
        case Some(tree: Tree) => locateEnclosing(tree :: base, pos)
        case none => base
      }
    }
  }

// --------- Copier/Transformer/Accumulator classes for untyped trees -----

  implicit class UntypedTreeCopier(val tree: Tree) extends AnyVal {
    def derivedModuleDef(mods: Modifiers, name: TermName, impl: Template) = tree match {
      case tree: ModuleDef if (mods eq tree.mods) && (name eq tree.name) && (impl eq tree.impl) =>tree
      case _ => ModuleDef(mods, name, impl).copyAttr(tree)
    }
    def derivedSymbolLit(str: String) = tree match {
      case tree: SymbolLit if (str == tree.str) => tree
      case _ => SymbolLit(str).copyAttr(tree)
    }
    def derivedInterpolatedString(id: TermName, strings: List[Literal], elems: List[Tree]) = tree match {
      case tree: InterpolatedString if (id eq tree.id) && (strings eq tree.strings) && (elems eq tree.elems) => tree
      case _ => InterpolatedString(id, strings, elems).copyAttr(tree)
    }
    def derivedFunction(args: List[Tree], body: Tree) = tree match {
      case tree: Function if (args eq tree.args) && (body eq tree.body) => tree
      case _ => Function(args, body).copyAttr(tree)
    }
    def derivedInfixOp(left: Tree, op: Name, right: Tree) = tree match {
      case tree: InfixOp if (left eq tree.left) && (op eq tree.op) && (right eq tree.right) => tree
      case _ => InfixOp(left, op, right).copyAttr(tree)
    }
    def derivedPostfixOp(od: Tree, op: Name) = tree match {
      case tree: PostfixOp if (od eq tree.od) && (op eq tree.op) => tree
      case _ => PostfixOp(od, op).copyAttr(tree)
    }
    def derivedPrefixOp(op: Name, od: Tree) = tree match {
      case tree: PrefixOp if (op eq tree.op) && (od eq tree.od) => tree
      case _ => PrefixOp(op, od).copyAttr(tree)
    }
    def derivedParens(t: Tree) = tree match {
      case tree: Parens if (t eq tree.t) => tree
      case _ => Parens(t).copyAttr(tree)
    }
    def derivedTuple(trees: List[Tree]) = tree match {
      case tree: Tuple if (trees eq tree.trees) => tree
      case _ => Tuple(trees).copyAttr(tree)
    }
    def derivedWhileDo(cond: Tree, body: Tree) = tree match {
      case tree: WhileDo if (cond eq tree.cond) && (body eq tree.body) => tree
      case _ => WhileDo(cond, body).copyAttr(tree)
    }
    def derivedDoWhile(body: Tree, cond: Tree) = tree match {
      case tree: DoWhile if (body eq tree.body) && (cond eq tree.cond) => tree
      case _ => DoWhile(body, cond).copyAttr(tree)
    }
    def derivedForYield(enums: List[Tree], expr: Tree) = tree match {
      case tree: ForYield if (enums eq tree.enums) && (expr eq tree.expr) => tree
      case _ => ForYield(enums, expr).copyAttr(tree)
    }
    def derivedForDo(enums: List[Tree], body: Tree) = tree match {
      case tree: ForDo if (enums eq tree.enums) && (body eq tree.body) => tree
      case _ => ForDo(enums, body).copyAttr(tree)
    }
    def derivedGenFrom(pat: Tree, expr: Tree) = tree match {
      case tree: GenFrom if (pat eq tree.pat) && (expr eq tree.expr) => tree
      case _ => GenFrom(pat, expr).copyAttr(tree)
    }
    def derivedGenAlias(pat: Tree, expr: Tree) = tree match {
      case tree: GenAlias if (pat eq tree.pat) && (expr eq tree.expr) => tree
      case _ => GenAlias(pat, expr).copyAttr(tree)
    }
    def derivedContextBounds(bounds: TypeBoundsTree, cxBounds: List[Tree]) = tree match {
      case tree: ContextBounds if (bounds eq tree.bounds) && (cxBounds eq tree.cxBounds) => tree
      case _ => ContextBounds(bounds, cxBounds).copyAttr(tree)
    }
    def derivedPatDef(mods: Modifiers, pats: List[Tree], tpt: Tree, rhs: Tree) = tree match {
      case tree: PatDef if (mods eq tree.mods) && (pats eq tree.pats) && (tpt eq tree.tpt) && (rhs eq tree.rhs) => tree
      case _ => PatDef(mods, pats, tpt, rhs).copyAttr(tree)
    }
  }

  abstract class TreeTransformer extends Trees.TreeTransformer[Untyped] {
    override def transform(tree: Tree): Tree = tree match {
      case ModuleDef(mods, name, impl) =>
        tree.derivedModuleDef(mods, name, transformSub(impl))
      case SymbolLit(str) =>
        tree.derivedSymbolLit(str)
      case InterpolatedString(id, strings, elems) =>
        tree.derivedInterpolatedString(id, transformSub(strings), transform(elems))
      case Function(args, body) =>
        tree.derivedFunction(transform(args), transform(body))
      case InfixOp(left, op, right) =>
        tree.derivedInfixOp(transform(left), op, transform(right))
      case PostfixOp(od, op) =>
        tree.derivedPostfixOp(transform(od), op)
      case PrefixOp(op, od) =>
        tree.derivedPrefixOp(op, transform(od))
      case Parens(t) =>
        tree.derivedParens(transform(t))
      case Tuple(trees) =>
        tree.derivedTuple(transform(trees))
      case WhileDo(cond, body) =>
        tree.derivedWhileDo(transform(cond), transform(body))
      case DoWhile(body, cond) =>
        tree.derivedDoWhile(transform(body), transform(cond))
      case ForYield(enums, expr) =>
        tree.derivedForYield(transform(enums), transform(expr))
      case ForDo(enums, body) =>
        tree.derivedForDo(transform(enums), transform(body))
      case GenFrom(pat, expr) =>
        tree.derivedGenFrom(transform(pat), transform(expr))
      case GenAlias(pat, expr) =>
        tree.derivedGenAlias(transform(pat), transform(expr))
      case ContextBounds(bounds, cxBounds) =>
        tree.derivedContextBounds(transformSub(bounds), transform(cxBounds))
      case PatDef(mods, pats, tpt, rhs) =>
        tree.derivedPatDef(mods, transform(pats), transform(tpt), transform(rhs))
      case _ =>
        super.transform(tree)
    }
  }

  abstract class TreeAccumulator[X] extends Trees.TreeAccumulator[X, Untyped] {
    override def foldOver(x: X, tree: Tree): X = tree match {
      case ModuleDef(mods, name, impl) =>
        this(x, impl)
      case SymbolLit(str) =>
        x
      case InterpolatedString(id, strings, elems) =>
        this(this(x, strings), elems)
      case Function(args, body) =>
        this(this(x, args), body)
      case InfixOp(left, op, right) =>
        this(this(x, left), right)
      case PostfixOp(od, op) =>
        this(x, od)
      case PrefixOp(op, od) =>
        this(x, od)
      case Parens(t) =>
        this(x, t)
      case Tuple(trees) =>
        this(x, trees)
      case WhileDo(cond, body) =>
        this(this(x, cond), body)
      case DoWhile(body, cond) =>
        this(this(x, body), cond)
      case ForYield(enums, expr) =>
        this(this(x, enums), expr)
      case ForDo(enums, body) =>
        this(this(x, enums), body)
      case GenFrom(pat, expr) =>
        this(this(x, pat), expr)
      case GenAlias(pat, expr) =>
        this(this(x, pat), expr)
      case ContextBounds(bounds, cxBounds) =>
        this(this(x, bounds), cxBounds)
      case PatDef(mods, pats, tpt, rhs) =>
        this(this(this(x, pats), tpt), rhs)
      case _ =>
        super.foldOver(x, tree)
    }
  }
}
