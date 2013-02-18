package dotty.tools.dotc.core

import Types._, Names._, Flags._, Positions._, Contexts._, Constants._, SymDenotations._, Symbols._
import Denotations._, StdNames._

object Trees {

  case class Modifiers[T](
    flags: FlagSet,
    privateWithin: TypeName = tpnme.EMPTY,
    annotations: List[Tree[T]] = Nil)

  /** Trees take a parameter indicating what the type of their `tpe` field
   *  is. Two choices: `Types.Type` or `missing.Type`.
   *  Untyped trees have type `Tree[missing.Type]`. Because `missing.Type`
   *  is a completely abstract type, there's nothing one can do with it.
   *
   *  Tree typing uses a copy-on-write implementation:
   *
   *   - You can never observe a `tpe` which is `null` (throws an exception)
   *   - So when creating a typed tree with `withType` we can re-use
   *     the existing tree transparently, assigning its `tpe` field,
   *     provided it was `null` before.
   *   - It is impossible to embed untyped trees in typed ones.
   *   - It is possible to embed typed trees in untyped ones. In fact
   *     there is an implicit conversion from `Tree[Types.Type]` to
   *     `Tree[missing.Type]` which wraps the typed tree in a
   *     `TypedSplice` node.
   *   - Type checking an untyped tree will remove all embedded `TypedSplice`
   *     nodes.
   */
  abstract class Tree[T] extends DotClass {
    def pos: Position

    type ThisTree[T] <: Tree[T]

    protected var _tpe: T = _

    def tpe: T = {
      if (_tpe == null) throw new UnAssignedTypeException(this)
      _tpe
    }

    def withType(tpe: Type): ThisTree[Type] = {
      val tree =
        (if (_tpe == null ||
            (_tpe.asInstanceOf[AnyRef] eq tpe.asInstanceOf[AnyRef])) this
         else clone).asInstanceOf[TypedTree]
      tree._tpe = tpe
      tree.asInstanceOf[ThisTree[Type]]
    }

    def denot(implicit ctx: Context): Denotation = NoDenotation
    def symbol(implicit ctx: Context): Symbol = NoSymbol

    def isType: Boolean = false
    def isTerm: Boolean = false
    def isDef: Boolean = false
    def isEmpty: Boolean = false
  }

  class UnAssignedTypeException[T](tree: Tree[T]) extends RuntimeException {
    override def getMessage: String = s"type of $tree is not assigned"
  }

  type TypedTree = Tree[Type]
  type UntypedTree = Tree[Nothing]

// ------ Categories of trees -----------------------------------

  /** Tree is definitely a type. Note that some trees
   *  have isType = true without being TypTrees (e.g. Ident, AnnotatedTree)
   */
  trait TypTree[T] extends Tree[T] {
    type ThisTree[T] <: TypTree[T]
    override def isType = true
  }

  /** Tree is definitely a term. Note that some trees
   *  have isType = true without being TypTrees (e.g. Ident, AnnotatedTree)
   */
  trait TermTree[T] extends Tree[T] {
    type ThisTree[T] <: TermTree[T]
    override def isTerm = true
  }

  /** Tree's symbol can be derived from its type */
  abstract class SymTree[T] extends Tree[T] {
    type ThisTree[T] <: SymTree[T]
    override def denot(implicit ctx: Context) = tpe match {
      case tpe: NamedType => tpe.denot
      case _ => NoDenotation
    }
    override def symbol(implicit ctx: Context): Symbol = tpe match {
      case tpe: Type => if (isType) tpe.typeSymbol else tpe.termSymbol
      case _ => NoSymbol
    }
  }

  /** Tree's symbol/isType/isTerm properties come from a subtree identifier
   *  by `forwardTo`.
   */
  abstract class ProxyTree[T] extends Tree[T] {
    type ThisTree[T] <: ProxyTree[T]
    def forwardTo: Tree[T]
    override def denot(implicit ctx: Context): Denotation = forwardTo.denot
    override def symbol(implicit ctx: Context): Symbol = forwardTo.symbol
    override def isTerm = forwardTo.isTerm
    override def isType = forwardTo.isType
  }

  /** Tree has a name */
  abstract class NameTree[T] extends SymTree[T] {
    type ThisTree[T] <: NameTree[T]
    def name: Name
  }

  /** Tree refers by name to a denotation */
  abstract class RefTree[T] extends NameTree[T] {
    type ThisTree[T] <: RefTree[T]
    def qualifier: Tree[T]
    override def isType = name.isTypeName
    override def isTerm = name.isTermName
  }

  abstract class DefTree[T] extends NameTree[T] {
    type ThisTree[T] <: DefTree[T]
    override def isDef = true
  }

// ----------- Tree case classes ------------------------------------

  /** name */
  case class Ident[T] (name: Name)(implicit val pos: Position)
    extends RefTree[T] {
    type ThisTree[T] = Ident[T]
    def qualifier: Tree[T] = EmptyTree[T]
  }

  /** qualifier.name */
  case class Select[T](qualifier: Tree[T], name: Name)(implicit val pos: Position)
    extends RefTree[T] {
    type ThisTree[T] = Select[T]
  }

  /** qual.this */
  case class This[T](qual: TypeName)(implicit val pos: Position)
    extends SymTree[T] with TermTree[T] {
    type ThisTree[T] = This[T]
  }

  /** C.super[mix], where qual = C.this */
  case class Super[T](qual: Tree[T], mix: TypeName)(implicit val pos: Position)
    extends ProxyTree[T] with TermTree[T] {
    type ThisTree[T] = Super[T]
    def forwardTo = qual
  }

  abstract class GenericApply[T] extends ProxyTree[T] with TermTree[T] {
    type ThisTree[T] <: GenericApply[T]
    val fun: Tree[T]
    val args: List[Tree[T]]
    def forwardTo = fun
  }

  /** fun(args) */
  case class Apply[T](fun: Tree[T], args: List[Tree[T]])(implicit val pos: Position)
    extends GenericApply[T] {
    type ThisTree[T] = Apply[T]
  }

  /** fun[args] */
  case class TypeApply[T](fun: Tree[T], args: List[Tree[T]])(implicit val pos: Position)
    extends GenericApply[T] {
    type ThisTree[T] = TypeApply[T]
  }

  /** const */
  case class Literal[T](const: Constant)(implicit val pos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Literal[T]
  }

  /** new tpt, but no constructor call */
  case class New[T](tpt: Tree[T])(implicit val pos: Position)
    extends TermTree[T] {
    type ThisTree[T] = New[T]
  }

  /** (left, right) */
  case class Pair[T](left: Tree[T], right: Tree[T])(implicit val pos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Pair[T]
  }

  /** expr : tpt */
  case class Typed[T](expr: Tree[T], tpt: Tree[T])(implicit val pos: Position)
    extends ProxyTree[T] with TermTree[T] {
    type ThisTree[T] = Typed[T]
    def forwardTo = expr
  }

  /** name = arg, in a parameter list */
  case class NamedArg[T](name: Name, arg: Tree[T])(implicit val pos: Position)
    extends TermTree[T] {
    type ThisTree[T] = NamedArg[T]
  }

  /** name = arg, outside a parameter list */
  case class Assign[T](lhs: Tree[T], rhs: Tree[T])(implicit val pos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Assign[T]
  }

  /** (vparams) => body */
  case class Function[T](vparams: List[ValDef[T]], body: Tree[T])(implicit val pos: Position)
    extends SymTree[T] with TermTree[T] {
    type ThisTree[T] = Function[T]
  }

  /** { stats; expr } */
  case class Block[T](stats: List[Tree[T]], expr: Tree[T])(implicit val pos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Block[T]
  }

  /** if cond then thenp else elsep */
  case class If[T](cond: Tree[T], thenp: Tree[T], elsep: Tree[T])(implicit val pos: Position)
    extends TermTree[T] {
    type ThisTree[T] = If[T]
  }

  /** selector match { cases } */
  case class Match[T](selector: Tree[T], cases: List[CaseDef[T]])(implicit val pos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Match[T]
  }

  /** case pat if guard => body */
  case class CaseDef[T](pat: Tree[T], guard: Tree[T], body: Tree[T])(implicit val pos: Position)
    extends Tree[T] {
    type ThisTree[T] = CaseDef[T]
  }

  /** return expr
   *  where `from` refers to the method from which the return takes place
   *  After program transformations this is not necessarily the enclosing method, because
   *  closures can intervene.
   */
  case class Return[T](expr: Tree[T], from: Ident[T])(implicit val pos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Return[T]
  }

  /** try block catch { catches } */
  case class Try[T](block: Tree[T], catches: List[CaseDef[T]], finalizer: Tree[T])(implicit val pos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Try[T]
  }

  /** throw expr */
  case class Throw[T](expr: Tree[T])(implicit val pos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Throw[T]
  }

  /** Array[elemtpt](elems) */
  case class ArrayValue[T](elemtpt: Tree[T], elems: List[Tree[T]])(implicit val pos: Position)
    extends TermTree[T] {
    type ThisTree[T] = ArrayValue[T]
  }

  /** A type tree that represents an existing or inferred type */
  case class TypeTree[T](original: Tree[T] = EmptyTree[T])(implicit val pos: Position)
    extends SymTree[T] with TypTree[T] {
    type ThisTree[T] = TypeTree[T]
  }

  /** ref.type */
  case class SingletonTypeTree[T](ref: Tree[T])(implicit val pos: Position)
    extends SymTree[T] with TypTree[T] {
    type ThisTree[T] = SingletonTypeTree[T]
  }

  /** qualifier # name */
  case class SelectFromTypeTree[T](qualifier: Tree[T], name: TypeName)(implicit val pos: Position)
    extends RefTree[T] with TypTree[T] {
    type ThisTree[T] = SelectFromTypeTree[T]
  }

  /** left & right */
  case class AndTypeTree[T](left: Tree[T], right: Tree[T])(implicit val pos: Position)
    extends TypTree[T] {
    type ThisTree[T] = AndTypeTree[T]
  }

  /** left | right */
  case class OrTypeTree[T](left: Tree[T], right: Tree[T])(implicit val pos: Position)
    extends TypTree[T] {
    type ThisTree[T] = OrTypeTree[T]
  }

  /** tpt { refinements } */
  case class RefineTypeTree[T](tpt: Tree[T], refinements: List[DefTree[T]])(implicit val pos: Position)
    extends ProxyTree[T] with TypTree[T] {
    type ThisTree[T] = RefineTypeTree[T]
    def forwardTo = tpt
  }

  /** tpt[args] */
  case class AppliedTypeTree[T](tpt: Tree[T], args: List[Tree[T]])(implicit val pos: Position)
    extends ProxyTree[T] with TypTree[T] {
    type ThisTree[T] = AppliedTypeTree[T]
    def forwardTo = tpt
  }

  /** >: lo <: hi */
  case class TypeBoundsTree[T](lo: Tree[T], hi: Tree[T])(implicit val pos: Position)
     extends Tree[T] {
    type ThisTree[T] = TypeBoundsTree[T]
  }

  /** name @ body */
  case class Bind[T](name: Name, body: Tree[T])(implicit val pos: Position)
    extends DefTree[T] {
    type ThisTree[T] = Bind[T]
  }

  /** tree_1 | ... | tree_n */
  case class Alternative[T](trees: List[Tree[T]])(implicit val pos: Position)
    extends Tree[T] {
    type ThisTree[T] = Alternative[T]
  }

  /** fun(args) in a pattern, if fun is an extractor */
  case class UnApply[T](fun: Tree[T], args: List[Tree[T]])(implicit val pos: Position)
    extends Tree[T] {
    type ThisTree[T] = UnApply[T]
  }

  /** mods val name: tpt = rhs */
  case class ValDef[T](mods: Modifiers[T], name: Name, tpt: Tree[T], rhs: Tree[T])(implicit val pos: Position)
    extends DefTree[T] {
    type ThisTree[T] = ValDef[T]
  }

  /** mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs */
  case class DefDef[T](mods: Modifiers[T], name: Name, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T])(implicit val pos: Position)
    extends DefTree[T] {
    type ThisTree[T] = DefDef[T]
  }

  class ImplicitDefDef[T](mods: Modifiers[T], name: Name, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T])
    (implicit pos: Position) extends DefDef[T](mods, name, tparams, vparamss, tpt, rhs) {
    override def copy[T](mods: Modifiers[T], name: Name, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T])(implicit pos: Position) =
      new ImplicitDefDef[T](mods, name, tparams, vparamss, tpt, rhs)
  }

  /** mods type name = rhs   or
   *  mods type name >: lo <: hi, if rhs = TypeBoundsTree(lo, hi) */
  case class TypeDef[T](mods: Modifiers[T], name: Name, rhs: Tree[T])(implicit val pos: Position)
    extends DefTree[T] {
    type ThisTree[T] = TypeDef[T]
  }

  /** extends parents { self => body } */
  case class Template[T](parents: List[Tree[T]], self: ValDef[T], body: List[Tree[T]])(implicit val pos: Position)
    extends SymTree[T] {
    type ThisTree[T] = Template[T]
  }

  /** mods class name[tparams] impl */
  case class ClassDef[T](mods: Modifiers[T], name: TypeName, tparams: List[TypeDef[T]], impl: Template[T])(implicit val pos: Position)
    extends DefTree[T] {
    type ThisTree[T] = ClassDef[T]
  }

  /** import expr.selectors
   *  where a selector is either an untyped `Ident`, `name` or
   *  an untyped `Pair` `name => rename`
   */
  case class Import[T](expr: Tree[T], selectors: List[UntypedTree])(implicit val pos: Position)
    extends SymTree[T] {
    type ThisTree[T] = Import[T]
  }

  /** package pid { stats } */
  case class PackageDef[T](pid: RefTree[T], stats: List[Tree[T]])(implicit val pos: Position)
    extends DefTree[T] {
    type ThisTree[T] = PackageDef[T]
    override def name = pid.name
  }

  /** arg @annot */
  case class Annotated[T](annot: Tree[T], arg: Tree[T])(implicit val pos: Position)
    extends ProxyTree[T] {
    type ThisTree[T] = Annotated[T]
    def forwardTo = arg
  }

  trait AlwaysEmpty[T] extends Tree[T] {
    override val pos = NoPosition
    override def tpe = unsupported("tpe")
    override def withType(tpe: Type) = unsupported("withType")
    override def isEmpty: Boolean = true
  }

  /** A missing tree */
  abstract case class EmptyTree[T]()
    extends Tree[T] with AlwaysEmpty[T] {
    type ThisTree[T] = EmptyTree[T]
  }

  private object theEmptyTree extends EmptyTree[Nothing]

  object EmptyTree {
    def apply[T]: EmptyTree[T] = theEmptyTree.asInstanceOf
  }

  class EmptyValDef[T] extends ValDef[T](
    Modifiers[T](Private), nme.WILDCARD, EmptyTree[T], EmptyTree[T])(NoPosition) with AlwaysEmpty[T]

  private object theEmptyValDef extends EmptyValDef[Nothing]

  object EmptyValDef {
    def apply[T]: EmptyValDef[T] = theEmptyValDef.asInstanceOf
  }

// ----- Tree cases that exist in untyped form only ------------------

  /** A typed subtree of an untyped tree needs to be wrapped in a TypedSlice */
  case class TypedSplice(tree: TypedTree) extends UntypedTree {
    def pos = tree.pos
  }

  /** mods object name impl */
  case class ModuleDef(mods: Modifiers[Nothing], name: TermName, impl: Template[Nothing])(implicit val pos: Position)
    extends DefTree[Nothing] {
  }

  abstract class TreeAccumulator[T, U] extends ((T, Tree[U]) => T) {
    def apply(x: T, tree: Tree[U]): T
    def foldOver(x: T, tree: Tree[U]): T = ???
  }
}