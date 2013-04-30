package dotty.tools.dotc
package core

import util.Positions._, Types._, Contexts._, Constants._, Names._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, TypedTrees._
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
    case class Function(args: List[Tree], body: Tree) extends Tree
    case class InfixOp(left: Tree, op: Name, right: Tree) extends Tree
    case class Postfixop(tree: Tree, op: Name) extends Tree
    case class Prefixop(op: Name, tree: Tree) extends Tree
    case class Parens(trees: List[Tree]) extends Tree
    case class WhileDo(cond: Tree, body: Tree) extends TermTree
    case class DoWhile(body: Tree, cond: Tree) extends TermTree
    case class ForYield(enums: List[Tree], expr: Tree) extends TermTree
    case class ForDo(enums: List[Tree], body: Tree) extends TermTree
    case class GenFrom(pat: Tree, expr: Tree) extends Tree
    case class GenAlias(pat: Tree, expr: Tree) extends Tree
    case class TypeParamBounds(below: Tree, above: Tree, view: Tree, context: Tree) extends TypTree
    case class PatDef(mods: Modifiers, pats: List[Tree], tpt: Tree, rhs: Tree) extends ModDefTree

    def Function(vparam: ValDef, body: Tree): Function =
      Function(vparam :: Nil, body)

    def syntheticParameter(pname: TermName): ValDef =
      ValDef(Modifiers(SyntheticTermParam), pname, TypeTree(), EmptyTree())
  }

  import untpd._

  class UGen(implicit ctx: Context) {
    def constructor(mods: Modifiers, vparamAccessorss: List[List[Tree]], ofTrait: Boolean): DefDef = ???
  }

  def ugen(implicit ctx: Context) =
    new UGen
}

