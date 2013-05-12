package dotty.tools.dotc
package core

import util.Positions._, Types._, Contexts._, Constants._, Names._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, TypedTrees._
import Decorators._
import language.higherKinds

object UntypedTrees {

  object untpd extends Trees.Instance[Untyped] {

    // ----- Tree cases that exist in untyped form only ------------------

    /** A typed subtree of an untyped tree needs to be wrapped in a TypedSlice */
    case class TypedSplice(tree: TypedTree) extends UntypedTree

    /** mods object name impl */
    case class ModuleDef(mods: Modifiers, name: TermName, impl: Template)
      extends NameTree with ModDefTree {
      type ThisTree[T >: Untyped] <: Trees.NameTree[T] with Trees.ModDefTree[T] with ModuleDef
    }

    /** (vparams) => body */
    case class SymbolLit(str: String) extends Tree
    case class InterpolatedString(id: TermName, stringParts: List[Tree], elems: List[Tree]) extends Tree
    case class Function(args: List[Tree], body: Tree) extends Tree
    case class InfixOp(left: Tree, op: Name, right: Tree) extends Tree
    case class PostfixOp(tree: Tree, op: Name) extends Tree
    case class PrefixOp(op: Name, tree: Tree) extends Tree
    case class Parens(tree: Tree) extends Tree
    case class Tuple(trees: List[Tree]) extends Tree
    case class WhileDo(cond: Tree, body: Tree) extends TermTree
    case class DoWhile(body: Tree, cond: Tree) extends TermTree
    case class ForYield(enums: List[Tree], expr: Tree) extends TermTree
    case class ForDo(enums: List[Tree], body: Tree) extends TermTree
    case class GenFrom(pat: Tree, expr: Tree) extends Tree
    case class GenAlias(pat: Tree, expr: Tree) extends Tree
    case class ContextBounds(bounds: TypeBoundsTree, cxBounds: List[Tree]) extends TypTree
    case class PatDef(mods: Modifiers, pats: List[Tree], tpt: Tree, rhs: Tree) extends Tree

    def Function(vparam: ValDef, body: Tree): Function =
      Function(vparam :: Nil, body)

    def syntheticParameter(pname: TermName): ValDef =
      ValDef(Modifiers(SyntheticTermParam), pname, TypeTree(), EmptyTree())

  }

  import untpd._

  class UGen(implicit ctx: Context) {
    def constructor(mods: Modifiers, vparamss: List[List[ValDef]], rhs: Tree = EmptyTree()): DefDef =
      DefDef(mods, nme.CONSTRUCTOR, Nil, vparamss, TypeTree(), rhs)

    def selfDef(name: TermName, tpt: Tree) =
      ValDef(Modifiers(Private), name, tpt, EmptyTree())

    def scalaDot(name: Name): Select =
      Select(new TypedSplice(tpd.Ident(defn.ScalaPackageVal.termRef)), name)

    def mkTuple(ts: List[Tree]) = ts match {
      case t :: Nil => Parens(t)
      case _ => Tuple(ts)
    }

    def scalaAnyRefConstr       = scalaDot(tpnme.AnyRef)
    def scalaAnyValConstr        = scalaDot(tpnme.AnyVal)
    def scalaAnyConstr           = scalaDot(tpnme.Any)
    def scalaUnitConstr          = scalaDot(tpnme.Unit)
    def productConstr            = scalaDot(tpnme.Product)
    def productConstrN(n: Int)   = scalaDot(("Product" + n).toTypeName)
    def serializableConstr       = scalaDot(tpnme.Serializable)
  }

  def ugen(implicit ctx: Context) =
    new UGen

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
}

