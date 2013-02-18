package dotty.tools.dotc.core

import Types._, Names._, Flags._, Positions._, Contexts._, Constants._, SymDenotations._, Symbols._
import Denotations._, StdNames._
import annotation.tailrec

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

    /** The tree's position. Except
     *  for Shared nodes, it is always ensured that a tree's position
     *  contains the positions of all its subtrees.
     */
    def pos: Position

    /** The typeconstructor at the root of the tree */
    type ThisTree[T] <: Tree[T]

    protected var _tpe: T = _

    /** The type of the tree. In case of an untyped tree,
     *   an UnAssignedTypeException is thrown.
     */
    def tpe: T = {
      if (_tpe == null) throw new UnAssignedTypeException(this)
      _tpe
    }

    /** Return a typed tree that's isomorphic to this tree, but has given
     *  type.
     */
    def withType(tpe: Type): ThisTree[Type] = {
      val tree =
        (if (_tpe == null ||
            (_tpe.asInstanceOf[AnyRef] eq tpe.asInstanceOf[AnyRef])) this
         else clone).asInstanceOf[TypedTree]
      tree._tpe = tpe
      tree.asInstanceOf[ThisTree[Type]]
    }

    /** The denotation referred to by this tree, NoDenotation where not applicable */
    def denot(implicit ctx: Context): Denotation = NoDenotation

    /** The symbol defined by ot referred to by this tree, NoSymbol where not applicable */
    def symbol(implicit ctx: Context): Symbol = NoSymbol

    /** Does this tree represent a type? */
    def isType: Boolean = false

    /** Does this tree represent a term? */
    def isTerm: Boolean = false

    /** Does this tree represent a definition or declaration? */
    def isDef: Boolean = false

    /** Is this tree either the empty tree or the empty ValDef? */
    def isEmpty: Boolean = false

    override def hashCode(): Int = System.identityHashCode(this)
    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
  }

  class UnAssignedTypeException[T](tree: Tree[T]) extends RuntimeException {
    override def getMessage: String = s"type of $tree is not assigned"
  }

  type TypedTree = Tree[Type]
  type UntypedTree = Tree[Nothing]

// ------ Categories of trees -----------------------------------

  /** Instances of this class are trees for which isType is definitely true.
   *  Note that some trees have isType = true without being TypTrees (e.g. Ident, AnnotatedTree)
   */
  trait TypTree[T] extends Tree[T] {
    type ThisTree[T] <: TypTree[T]
    override def isType = true
  }

  /** Instances of this class are trees for which isTerm is definitely true.
   *  Note that some trees have isTerm = true without being TermTrees (e.g. Ident, AnnotatedTree)
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

  /** Tree's symbol/isType/isTerm properties come from a subtree identified
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

  /** Tree represents a definition */
  abstract class DefTree[T] extends NameTree[T] {
    type ThisTree[T] <: DefTree[T]
    override def isDef = true
  }

// ----------- Tree case classes ------------------------------------

  /** name */
  case class Ident[T] (name: Name)(implicit cpos: Position)
    extends RefTree[T] {
    type ThisTree[T] = Ident[T]
    val pos = cpos
    def qualifier: Tree[T] = EmptyTree[T]
  }

  /** qualifier.name */
  case class Select[T](qualifier: Tree[T], name: Name)(implicit cpos: Position)
    extends RefTree[T] {
    type ThisTree[T] = Select[T]
    val pos = cpos union qualifier.pos
  }

  /** qual.this */
  case class This[T](qual: TypeName)(implicit cpos: Position)
    extends SymTree[T] with TermTree[T] {
    type ThisTree[T] = This[T]
    val pos = cpos
  }

  /** C.super[mix], where qual = C.this */
  case class Super[T](qual: Tree[T], mix: TypeName)(implicit cpos: Position)
    extends ProxyTree[T] with TermTree[T] {
    type ThisTree[T] = Super[T]
    val pos = cpos union qual.pos
    def forwardTo = qual
  }

  abstract class GenericApply[T] extends ProxyTree[T] with TermTree[T] {
    type ThisTree[T] <: GenericApply[T]
    val fun: Tree[T]
    val args: List[Tree[T]]
    def forwardTo = fun
  }

  /** fun(args) */
  case class Apply[T](fun: Tree[T], args: List[Tree[T]])(implicit cpos: Position)
    extends GenericApply[T] {
    type ThisTree[T] = Apply[T]
    val pos = unionPos(cpos union fun.pos, args)
  }

  /** fun[args] */
  case class TypeApply[T](fun: Tree[T], args: List[Tree[T]])(implicit cpos: Position)
    extends GenericApply[T] {
    type ThisTree[T] = TypeApply[T]
    val pos = unionPos(cpos union fun.pos, args)
  }

  /** const */
  case class Literal[T](const: Constant)(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Literal[T]
    val pos = cpos
  }

  /** new tpt, but no constructor call */
  case class New[T](tpt: Tree[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T] = New[T]
    val pos = cpos union tpt.pos
  }

  /** (left, right) */
  case class Pair[T](left: Tree[T], right: Tree[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Pair[T]
    val pos = cpos union left.pos union right.pos
  }

  /** expr : tpt */
  case class Typed[T](expr: Tree[T], tpt: Tree[T])(implicit cpos: Position)
    extends ProxyTree[T] with TermTree[T] {
    type ThisTree[T] = Typed[T]
    val pos = cpos union expr.pos union tpt.pos
    def forwardTo = expr
  }

  /** name = arg, in a parameter list */
  case class NamedArg[T](name: Name, arg: Tree[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T] = NamedArg[T]
    val pos = cpos union arg.pos
  }

  /** name = arg, outside a parameter list */
  case class Assign[T](lhs: Tree[T], rhs: Tree[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Assign[T]
    val pos = cpos union lhs.pos union rhs.pos
  }

  /** (vparams) => body */
  case class Function[T](vparams: List[ValDef[T]], body: Tree[T])(implicit cpos: Position)
    extends SymTree[T] with TermTree[T] {
    type ThisTree[T] = Function[T]
    val pos = unionPos(cpos union body.pos, vparams)
  }

  /** { stats; expr } */
  case class Block[T](stats: List[Tree[T]], expr: Tree[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Block[T]
    val pos = unionPos(cpos union expr.pos, stats)
  }

  /** if cond then thenp else elsep */
  case class If[T](cond: Tree[T], thenp: Tree[T], elsep: Tree[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T] = If[T]
    val pos = cpos union cond.pos union thenp.pos union elsep.pos
  }

  /** selector match { cases } */
  case class Match[T](selector: Tree[T], cases: List[CaseDef[T]])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Match[T]
    val pos = unionPos(cpos union selector.pos, cases)
  }

  /** case pat if guard => body */
  case class CaseDef[T](pat: Tree[T], guard: Tree[T], body: Tree[T])(implicit cpos: Position)
    extends Tree[T] {
    type ThisTree[T] = CaseDef[T]
    val pos = cpos union pat.pos union guard.pos union body.pos
  }

  /** return expr
   *  where `from` refers to the method from which the return takes place
   *  After program transformations this is not necessarily the enclosing method, because
   *  closures can intervene.
   */
  case class Return[T](expr: Tree[T], from: Ident[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Return[T]
    val pos = cpos union expr.pos // from is synthetic, does not influence pos
  }

  /** try block catch { catches } */
  case class Try[T](block: Tree[T], catches: List[CaseDef[T]], finalizer: Tree[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Try[T]
    val pos = unionPos(cpos union block.pos union finalizer.pos, catches)
  }

  /** throw expr */
  case class Throw[T](expr: Tree[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Throw[T]
    val pos = cpos union expr.pos
  }

  /** Array[elemtpt](elems) */
  case class ArrayValue[T](elemtpt: Tree[T], elems: List[Tree[T]])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T] = ArrayValue[T]
    val pos = unionPos(cpos union elemtpt.pos, elems)
  }

  /** A type tree that represents an existing or inferred type */
  case class TypeTree[T](original: Tree[T] = EmptyTree[T])(implicit cpos: Position)
    extends SymTree[T] with TypTree[T] {
    type ThisTree[T] = TypeTree[T]
    val pos = cpos union original.pos
  }

  /** ref.type */
  case class SingletonTypeTree[T](ref: Tree[T])(implicit cpos: Position)
    extends SymTree[T] with TypTree[T] {
    type ThisTree[T] = SingletonTypeTree[T]
    val pos = cpos union ref.pos
  }

  /** qualifier # name */
  case class SelectFromTypeTree[T](qualifier: Tree[T], name: TypeName)(implicit cpos: Position)
    extends RefTree[T] with TypTree[T] {
    type ThisTree[T] = SelectFromTypeTree[T]
    val pos = cpos union qualifier.pos
  }

  /** left & right */
  case class AndTypeTree[T](left: Tree[T], right: Tree[T])(implicit cpos: Position)
    extends TypTree[T] {
    type ThisTree[T] = AndTypeTree[T]
    val pos = cpos union left.pos union right.pos
  }

  /** left | right */
  case class OrTypeTree[T](left: Tree[T], right: Tree[T])(implicit cpos: Position)
    extends TypTree[T] {
    type ThisTree[T] = OrTypeTree[T]
    val pos = cpos union left.pos union right.pos
  }

  /** tpt { refinements } */
  case class RefineTypeTree[T](tpt: Tree[T], refinements: List[DefTree[T]])(implicit cpos: Position)
    extends ProxyTree[T] with TypTree[T] {
    type ThisTree[T] = RefineTypeTree[T]
    val pos = unionPos(cpos union tpt.pos, refinements)
    def forwardTo = tpt
  }

  /** tpt[args] */
  case class AppliedTypeTree[T](tpt: Tree[T], args: List[Tree[T]])(implicit cpos: Position)
    extends ProxyTree[T] with TypTree[T] {
    type ThisTree[T] = AppliedTypeTree[T]
    val pos = unionPos(cpos union tpt.pos, args)
    def forwardTo = tpt
  }

  /** >: lo <: hi */
  case class TypeBoundsTree[T](lo: Tree[T], hi: Tree[T])(implicit cpos: Position)
    extends Tree[T] {
    type ThisTree[T] = TypeBoundsTree[T]
    val pos = cpos union lo.pos union hi.pos
  }

  /** name @ body */
  case class Bind[T](name: Name, body: Tree[T])(implicit cpos: Position)
    extends DefTree[T] {
    type ThisTree[T] = Bind[T]
    val pos = cpos union body.pos
  }

  /** tree_1 | ... | tree_n */
  case class Alternative[T](trees: List[Tree[T]])(implicit cpos: Position)
    extends Tree[T] {
    type ThisTree[T] = Alternative[T]
    val pos = unionPos(cpos, trees)
  }

  /** fun(args) in a pattern, if fun is an extractor */
  case class UnApply[T](fun: Tree[T], args: List[Tree[T]])(implicit cpos: Position)
    extends Tree[T] {
    type ThisTree[T] = UnApply[T]
    val pos = unionPos(cpos union fun.pos, args)
  }

  /** mods val name: tpt = rhs */
  case class ValDef[T](mods: Modifiers[T], name: Name, tpt: Tree[T], rhs: Tree[T])(implicit cpos: Position)
    extends DefTree[T] {
    type ThisTree[T] = ValDef[T]
    val pos = cpos union tpt.pos union rhs.pos
  }

  /** mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs */
  case class DefDef[T](mods: Modifiers[T], name: Name, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T])(implicit cpos: Position)
    extends DefTree[T] {
    type ThisTree[T] = DefDef[T]
    val pos = (unionPos(cpos union tpt.pos union rhs.pos, tparams) /: vparamss)(unionPos)
  }

  class ImplicitDefDef[T](mods: Modifiers[T], name: Name, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T])
    (implicit pos: Position) extends DefDef[T](mods, name, tparams, vparamss, tpt, rhs) {
    override def copy[T](mods: Modifiers[T], name: Name, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T])(implicit pos: Position) =
      new ImplicitDefDef[T](mods, name, tparams, vparamss, tpt, rhs)
  }

  /** mods type name = rhs   or
   *  mods type name >: lo <: hi, if rhs = TypeBoundsTree(lo, hi) */
  case class TypeDef[T](mods: Modifiers[T], name: Name, rhs: Tree[T])(implicit cpos: Position)
    extends DefTree[T] {
    type ThisTree[T] = TypeDef[T]
    val pos = cpos union rhs.pos
  }

  /** extends parents { self => body } */
  case class Template[T](parents: List[Tree[T]], self: ValDef[T], body: List[Tree[T]])(implicit cpos: Position)
    extends SymTree[T] {
    type ThisTree[T] = Template[T]
    val pos = unionPos(unionPos(cpos union self.pos, parents), body)
  }

  /** mods class name[tparams] impl */
  case class ClassDef[T](mods: Modifiers[T], name: TypeName, tparams: List[TypeDef[T]], impl: Template[T])(implicit cpos: Position)
    extends DefTree[T] {
    type ThisTree[T] = ClassDef[T]
    val pos = unionPos(cpos union impl.pos, tparams)
  }

  /** import expr.selectors
   *  where a selector is either an untyped `Ident`, `name` or
   *  an untyped `Pair` `name => rename`
   */
  case class Import[T](expr: Tree[T], selectors: List[UntypedTree])(implicit cpos: Position)
    extends SymTree[T] {
    type ThisTree[T] = Import[T]
    val pos = unionPos(cpos union expr.pos, selectors)
  }

  /** package pid { stats } */
  case class PackageDef[T](pid: RefTree[T], stats: List[Tree[T]])(implicit cpos: Position)
    extends DefTree[T] {
    type ThisTree[T] = PackageDef[T]
    val pos = unionPos(cpos union pid.pos, stats)
    override def name = pid.name
  }

  /** arg @annot */
  case class Annotated[T](annot: Tree[T], arg: Tree[T])(implicit cpos: Position)
    extends ProxyTree[T] {
    type ThisTree[T] = Annotated[T]
    val pos = cpos union annot.pos union arg.pos
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

  /** A tree that can be shared without its position polluting containing trees */
  case class Shared[T](tree: Tree[T]) extends Tree[T] {
    type ThisTree[T] = Shared[T]
    val pos = NoPosition
  }

  // ----- Tree cases that exist in untyped form only ------------------

  /** A typed subtree of an untyped tree needs to be wrapped in a TypedSlice */
  class TypedSplice(tree: TypedTree) extends UntypedTree {
    val pos = tree.pos
  }

  /** mods object name impl */
  case class ModuleDef(mods: Modifiers[Nothing], name: TermName, impl: Template[Nothing])(implicit cpos: Position)
    extends DefTree[Nothing] {
    val pos = cpos union impl.pos
  }

// ----- Helper functions and classes ---------------------------------------

  @tailrec final def unionPos(base: Position, trees: List[Tree[_]]): Position = trees match {
    case t :: ts => unionPos(base union t.pos, ts)
    case nil => base
  }

  implicit class TreeCopier[T](val tree: Tree[T]) extends AnyVal {
    implicit def cpos = tree.pos
    def derivedIdent(name: Name): Ident[T] = tree match {
      case tree: Ident[_] if (name == tree.name) => tree
      case _ => Trees.Ident(name)
    }
    def derivedSelect(qualifier: Tree[T], name: Name): Select[T] = tree match {
      case tree: Select[_] if (qualifier eq tree.qualifier) && (name == tree.name) => tree
      case _ => Trees.Select(qualifier, name)
    }
    def derivedThis(qual: TypeName): This[T] = tree match {
      case tree: This[_] if (qual == tree.qual) => tree
      case _ => Trees.This(qual)
    }
    def derivedSuper(qual: Tree[T], mix: TypeName): Super[T] = tree match {
      case tree: Super[_] if (qual eq tree.qual) && (mix == tree.mix) => tree
      case _ => Trees.Super(qual, mix)
    }
    def derivedApply(fun: Tree[T], args: List[Tree[T]]): Apply[T] = tree match {
      case tree: Apply[_] if (fun eq tree.fun) && (args eq tree.args) => tree
      case _ => Trees.Apply(fun, args)
    }
    def derivedTypeApply(fun: Tree[T], args: List[Tree[T]]): TypeApply[T] = tree match {
      case tree: TypeApply[_] if (fun eq tree.fun) && (args eq tree.args) => tree
      case _ => Trees.TypeApply(fun, args)
    }
    def derivedLiteral(const: Constant): Literal[T] = tree match {
      case tree: Literal[_] if (const == tree.const) => tree
      case _ => Trees.Literal(const)
    }
    def derivedNew(tpt: Tree[T]): New[T] = tree match {
      case tree: New[_] if (tpt eq tree.tpt) => tree
      case _ => Trees.New(tpt)
    }
    def derivedPair(left: Tree[T], right: Tree[T]): Pair[T] = tree match {
      case tree: Pair[_] if (left eq tree.left) && (right eq tree.right) => tree
      case _ => Trees.Pair(left, right)
    }
    def derivedTyped(expr: Tree[T], tpt: Tree[T]): Typed[T] = tree match {
      case tree: Typed[_] if (expr eq tree.expr) && (tpt eq tree.tpt) => tree
      case _ => Trees.Typed(expr, tpt)
    }
    def derivedNamedArg(name: Name, arg: Tree[T]): NamedArg[T] = tree match {
      case tree: NamedArg[_] if (name == tree.name) && (arg eq tree.arg) => tree
      case _ => Trees.NamedArg(name, arg)
    }
    def derivedAssign(lhs: Tree[T], rhs: Tree[T]): Assign[T] = tree match {
      case tree: Assign[_] if (lhs eq tree.lhs) && (rhs eq tree.rhs) => tree
      case _ => Trees.Assign(lhs, rhs)
    }
    def derivedFunction(vparams: List[ValDef[T]], body: Tree[T]): Function[T] = tree match {
      case tree: Function[_] if (vparams eq tree.vparams) && (body eq tree.body) => tree
      case _ => Trees.Function(vparams, body)
    }
    def derivedBlock(stats: List[Tree[T]], expr: Tree[T]): Block[T] = tree match {
      case tree: Block[_] if (stats eq tree.stats) && (expr eq tree.expr) => tree
      case _ => Trees.Block(stats, expr)
    }
    def derivedIf(cond: Tree[T], thenp: Tree[T], elsep: Tree[T]): If[T] = tree match {
      case tree: If[_] if (cond eq tree.cond) && (thenp eq tree.thenp) && (elsep eq tree.elsep) => tree
      case _ => Trees.If(cond, thenp, elsep)
    }
    def derivedMatch(selector: Tree[T], cases: List[CaseDef[T]]): Match[T] = tree match {
      case tree: Match[_] if (selector eq tree.selector) && (cases eq tree.cases) => tree
      case _ => Trees.Match(selector, cases)
    }
    def derivedCaseDef(pat: Tree[T], guard: Tree[T], body: Tree[T]): CaseDef[T] = tree match {
      case tree: CaseDef[_] if (pat eq tree.pat) && (guard eq tree.guard) && (body eq tree.body) => tree
      case _ => Trees.CaseDef(pat, guard, body)
    }
    def derivedReturn(expr: Tree[T], from: Ident[T]): Return[T] = tree match {
      case tree: Return[_] if (expr eq tree.expr) && (from eq tree.from) => tree
      case _ => Trees.Return(expr, from)
    }
    def derivedTry(block: Tree[T], catches: List[CaseDef[T]], finalizer: Tree[T]): Try[T] = tree match {
      case tree: Try[_] if (block eq tree.block) && (catches eq tree.catches) && (finalizer eq tree.finalizer) => tree
      case _ => Trees.Try(block, catches, finalizer)
    }
    def derivedThrow(expr: Tree[T]): Throw[T] = tree match {
      case tree: Throw[_] if (expr eq tree.expr) => tree
      case _ => Trees.Throw(expr)
    }
    def derivedArrayValue(elemtpt: Tree[T], elems: List[Tree[T]]): ArrayValue[T] = tree match {
      case tree: ArrayValue[_] if (elemtpt eq tree.elemtpt) && (elems eq tree.elems) => tree
      case _ => Trees.ArrayValue(elemtpt, elems)
    }
    def derivedTypeTree(original: Tree[T] = EmptyTree[T]): TypeTree[T] = tree match {
      case tree: TypeTree[_] if (original eq tree.original) => tree
      case _ => Trees.TypeTree(original)
    }
    def derivedSingletonTypeTree(ref: Tree[T]): SingletonTypeTree[T] = tree match {
      case tree: SingletonTypeTree[_] if (ref eq tree.ref) => tree
      case _ => Trees.SingletonTypeTree(ref)
    }
    def derivedSelectFromTypeTree(qualifier: Tree[T], name: TypeName): SelectFromTypeTree[T] = tree match {
      case tree: SelectFromTypeTree[_] if (qualifier eq tree.qualifier) && (name == tree.name) => tree
      case _ => Trees.SelectFromTypeTree(qualifier, name)
    }
    def derivedAndTypeTree(left: Tree[T], right: Tree[T]): AndTypeTree[T] = tree match {
      case tree: AndTypeTree[_] if (left eq tree.left) && (right eq tree.right) => tree
      case _ => Trees.AndTypeTree(left, right)
    }
    def derivedOrTypeTree(left: Tree[T], right: Tree[T]): OrTypeTree[T] = tree match {
      case tree: OrTypeTree[_] if (left eq tree.left) && (right eq tree.right) => tree
      case _ => Trees.OrTypeTree(left, right)
    }
    def derivedRefineTypeTree(tpt: Tree[T], refinements: List[DefTree[T]]): RefineTypeTree[T] = tree match {
      case tree: RefineTypeTree[_] if (tpt eq tree.tpt) && (refinements eq tree.refinements) => tree
      case _ => Trees.RefineTypeTree(tpt, refinements)
    }
    def derivedAppliedTypeTree(tpt: Tree[T], args: List[Tree[T]]): AppliedTypeTree[T] = tree match {
      case tree: AppliedTypeTree[_] if (tpt eq tree.tpt) && (args eq tree.args) => tree
      case _ => Trees.AppliedTypeTree(tpt, args)
    }
    def derivedTypeBoundsTree(lo: Tree[T], hi: Tree[T]): TypeBoundsTree[T] = tree match {
      case tree: TypeBoundsTree[_] if (lo eq tree.lo) && (hi eq tree.hi) => tree
      case _ => Trees.TypeBoundsTree(lo, hi)
    }
    def derivedBind(name: Name, body: Tree[T]): Bind[T] = tree match {
      case tree: Bind[_] if (name eq tree.name) && (body eq tree.body) => tree
      case _ => Trees.Bind(name, body)
    }
    def derivedAlternative(trees: List[Tree[T]]): Alternative[T] = tree match {
      case tree: Alternative[_] if (trees eq tree.trees) => tree
      case _ => Trees.Alternative(trees)
    }
    def derivedUnApply(fun: Tree[T], args: List[Tree[T]]): UnApply[T] = tree match {
      case tree: UnApply[_] if (fun eq tree.fun) && (args eq tree.args) => tree
      case _ => Trees.UnApply(fun, args)
    }
    def derivedValDef(mods: Modifiers[T], name: Name, tpt: Tree[T], rhs: Tree[T]): ValDef[T] = tree match {
      case tree: ValDef[_] if (mods == tree.mods) && (name == tree.name) && (tpt eq tree.tpt) && (rhs eq tree.rhs) => tree
      case _ => Trees.ValDef(mods, name, tpt, rhs)
    }
    def derivedDefDef(mods: Modifiers[T], name: Name, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T]): DefDef[T] = tree match {
      case tree: DefDef[_] if (mods == tree.mods) && (name == tree.name) && (tparams eq tree.tparams) && (vparamss eq tree.vparamss) && (tpt eq tree.tpt) && (rhs eq tree.rhs) => tree
      case _ => Trees.DefDef(mods, name, tparams, vparamss, tpt, rhs)
    }
    def derivedTypeDef(mods: Modifiers[T], name: Name, rhs: Tree[T]): TypeDef[T] = tree match {
      case tree: TypeDef[_] if (mods == tree.mods) && (name == tree.name) && (rhs eq tree.rhs) => tree
      case _ => Trees.TypeDef(mods, name, rhs)
    }
    def derivedTemplate(parents: List[Tree[T]], self: ValDef[T], body: List[Tree[T]]): Template[T] = tree match {
      case tree: Template[_] if (parents eq tree.parents) && (self eq tree.self) && (body eq tree.body) => tree
      case _ => Trees.Template(parents, self, body)
    }
    def derivedClassDef(mods: Modifiers[T], name: TypeName, tparams: List[TypeDef[T]], impl: Template[T]): ClassDef[T] = tree match {
      case tree: ClassDef[_] if (mods == tree.mods) && (name == tree.name) && (tparams eq tree.tparams) && (impl eq tree.impl) => tree
      case _ => Trees.ClassDef(mods, name, tparams, impl)
    }
    def derivedImport(expr: Tree[T], selectors: List[UntypedTree]): Import[T] = tree match {
      case tree: Import[_] if (expr eq tree.expr) && (selectors eq tree.selectors) => tree
      case _ => Trees.Import(expr, selectors)
    }
    def derivedPackageDef(pid: RefTree[T], stats: List[Tree[T]]): PackageDef[T] = tree match {
      case tree: PackageDef[_] if (pid eq tree.pid) && (stats eq tree.stats) => tree
      case _ => Trees.PackageDef(pid, stats)
    }
    def derivedAnnotated(annot: Tree[T], arg: Tree[T]): Annotated[T] = tree match {
      case tree: Annotated[_] if (annot eq tree.annot) && (arg eq tree.arg) => tree
      case _ => Trees.Annotated(annot, arg)
    }
  }

  abstract class TreeAccumulator[T, U] extends ((T, Tree[U]) => T) {
    def apply(x: T, tree: Tree[U]): T
    def foldOver(x: T, tree: Tree[U]): T = ???
  }
}
