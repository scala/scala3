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

    private var _tpe: T = _

    /** The type of the tree. In case of an untyped tree,
     *   an UnAssignedTypeException is thrown.
     */
    def tpe: T = {
      if (_tpe == null) throw new UnAssignedTypeException(this)
      _tpe
    }

    /** Copy `tpe` attribute from tree `from` into this tree, independently
     *  whether it is null or not.
     */
    def copyAttr(from: Tree[T]): ThisTree[T] = {
      _tpe = from._tpe
      this.asInstanceOf[ThisTree[T]]
    }

    /** Return a typed tree that's isomorphic to this tree, but has given
     *  type.
     */
    def withType(tpe: Type): ThisTree[Type] = {
      val tree =
        (if (_tpe == null ||
          (_tpe.asInstanceOf[AnyRef] eq tpe.asInstanceOf[AnyRef])) this
        else clone).asInstanceOf[Tree[Type]]
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

    /** Is this a legal part of a pattern which is not at the same time a term? */
    def isPattern: Boolean = false

    /** Does this tree define a new symbol that is not defined elsewhere? */
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

  /** Instances of this class are trees which are not terms but are legal
   *  parts of patterns.
   */
  trait PatternTree[T] extends Tree[T] {
    type ThisTree[T] <: PatternTree[T]
    override def isPattern = true
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

  /** Tree defines a new symbol */
  trait DefTree[T] extends SymTree[T] {
    type ThisTree[T] <: DefTree[T]
    override def isDef = true
  }

  // ----------- Tree case classes ------------------------------------

  /** name */
  case class Ident[T](name: Name)(implicit cpos: Position)
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
    extends Tree[T] {
    type ThisTree[T] = NamedArg[T]
    val pos = cpos union arg.pos
  }

  /** name = arg, outside a parameter list */
  case class Assign[T](lhs: Tree[T], rhs: Tree[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T] = Assign[T]
    val pos = cpos union lhs.pos union rhs.pos
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
  case class SeqLiteral[T](elemtpt: Tree[T], elems: List[Tree[T]])(implicit cpos: Position)
    extends   Tree[T] {
    type ThisTree[T] = SeqLiteral[T]
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
  case class SelectFromTypeTree[T](qualifier: Tree[T], name: Name)(implicit cpos: Position)
    extends RefTree[T] {
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
    extends NameTree[T] with DefTree[T] with PatternTree[T] {
    type ThisTree[T] = Bind[T]
    val pos = cpos union body.pos
  }

  /** tree_1 | ... | tree_n */
  case class Alternative[T](trees: List[Tree[T]])(implicit cpos: Position)
    extends Tree[T] with PatternTree[T] {
    type ThisTree[T] = Alternative[T]
    val pos = unionPos(cpos, trees)
  }

  /** fun(args) in a pattern, if fun is an extractor */
  case class UnApply[T](fun: Tree[T], args: List[Tree[T]])(implicit cpos: Position)
    extends Tree[T] with PatternTree[T] {
    type ThisTree[T] = UnApply[T]
    val pos = unionPos(cpos union fun.pos, args)
  }

  /** mods val name: tpt = rhs */
  case class ValDef[T](mods: Modifiers[T], name: TermName, tpt: Tree[T], rhs: Tree[T])(implicit cpos: Position)
    extends NameTree[T] with DefTree[T] {
    type ThisTree[T] = ValDef[T]
    val pos = cpos union tpt.pos union rhs.pos
  }

  /** mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs */
  case class DefDef[T](mods: Modifiers[T], name: TermName, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T])(implicit cpos: Position)
    extends NameTree[T] with DefTree[T] {
    type ThisTree[T] = DefDef[T]
    val pos = (unionPos(cpos union tpt.pos union rhs.pos, tparams) /: vparamss)(unionPos)
  }

  class ImplicitDefDef[T](mods: Modifiers[T], name: TermName, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T])(implicit pos: Position) extends DefDef[T](mods, name, tparams, vparamss, tpt, rhs) {
    override def copy[T](mods: Modifiers[T], name: TermName, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T])(implicit pos: Position) =
      new ImplicitDefDef[T](mods, name, tparams, vparamss, tpt, rhs)
  }

  /** mods type name = rhs   or
   *  mods type name >: lo <: hi, if rhs = TypeBoundsTree(lo, hi)
   */
  case class TypeDef[T](mods: Modifiers[T], name: TypeName, rhs: Tree[T])(implicit cpos: Position)
    extends NameTree[T] with DefTree[T] {
    type ThisTree[T] = TypeDef[T]
    val pos = cpos union rhs.pos
  }

  /** extends parents { self => body } */
  case class Template[T](parents: List[Tree[T]], self: ValDef[T], body: List[Tree[T]])(implicit cpos: Position)
    extends DefTree[T] {
    type ThisTree[T] = Template[T]
    val pos = unionPos(unionPos(cpos union self.pos, parents), body)
  }

  /** mods class name[tparams] impl */
  case class ClassDef[T](mods: Modifiers[T], name: TypeName, tparams: List[TypeDef[T]], impl: Template[T])(implicit cpos: Position)
    extends NameTree[T] with DefTree[T] {
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
    extends ProxyTree[T] {
    type ThisTree[T] = PackageDef[T]
    val pos = unionPos(cpos union pid.pos, stats)
    def forwardTo = pid
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

  /** A tree that can be shared without its position
   *  polluting containing trees. Accumulators and tranformers
   *  memoize results of shared subtrees
   */
  case class Shared[T](shared: Tree[T]) extends Tree[T] {
    type ThisTree[T] = Shared[T]
    val pos = NoPosition
    override val isTerm = shared.isTerm
    override val isType = shared.isType
  }

  // ----- Tree cases that exist in untyped form only ------------------

  /** A typed subtree of an untyped tree needs to be wrapped in a TypedSlice */
  class TypedSplice(tree: TypedTree) extends UntypedTree {
    val pos = tree.pos
  }

  /** mods object name impl */
  case class ModuleDef(mods: Modifiers[Nothing], name: TermName, impl: Template[Nothing])(implicit cpos: Position)
    extends NameTree[Nothing] with DefTree[Nothing] {
    type ThisTree[T] <: NameTree[T] with DefTree[T] with ModuleDef
    val pos = cpos union impl.pos
  }

  /** (vparams) => body */
  case class Function(vparams: List[ValDef[Nothing]], body: Tree[Nothing])(implicit cpos: Position)
    extends TermTree[Nothing] {
    type ThisTree[T] <: TermTree[T] with Function
    val pos = unionPos(cpos union body.pos, vparams)
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
      case _ => Ident(name).copyAttr(tree)
    }
    def derivedSelect(qualifier: Tree[T], name: Name): Select[T] = tree match {
      case tree: Select[_] if (qualifier eq tree.qualifier) && (name == tree.name) => tree
      case _ => Select(qualifier, name).copyAttr(tree)
    }
    def derivedThis(qual: TypeName): This[T] = tree match {
      case tree: This[_] if (qual == tree.qual) => tree
      case _ => This(qual).copyAttr(tree)
    }
    def derivedSuper(qual: Tree[T], mix: TypeName): Super[T] = tree match {
      case tree: Super[_] if (qual eq tree.qual) && (mix == tree.mix) => tree
      case _ => Super(qual, mix).copyAttr(tree)
    }
    def derivedApply(fun: Tree[T], args: List[Tree[T]]): Apply[T] = tree match {
      case tree: Apply[_] if (fun eq tree.fun) && (args eq tree.args) => tree
      case _ => Apply(fun, args).copyAttr(tree)
    }
    def derivedTypeApply(fun: Tree[T], args: List[Tree[T]]): TypeApply[T] = tree match {
      case tree: TypeApply[_] if (fun eq tree.fun) && (args eq tree.args) => tree
      case _ => TypeApply(fun, args).copyAttr(tree)
    }
    def derivedLiteral(const: Constant): Literal[T] = tree match {
      case tree: Literal[_] if (const == tree.const) => tree
      case _ => Literal(const).copyAttr(tree)
    }
    def derivedNew(tpt: Tree[T]): New[T] = tree match {
      case tree: New[_] if (tpt eq tree.tpt) => tree
      case _ => New(tpt).copyAttr(tree)
    }
    def derivedPair(left: Tree[T], right: Tree[T]): Pair[T] = tree match {
      case tree: Pair[_] if (left eq tree.left) && (right eq tree.right) => tree
      case _ => Pair(left, right).copyAttr(tree)
    }
    def derivedTyped(expr: Tree[T], tpt: Tree[T]): Typed[T] = tree match {
      case tree: Typed[_] if (expr eq tree.expr) && (tpt eq tree.tpt) => tree
      case _ => Typed(expr, tpt).copyAttr(tree)
    }
    def derivedNamedArg(name: Name, arg: Tree[T]): NamedArg[T] = tree match {
      case tree: NamedArg[_] if (name == tree.name) && (arg eq tree.arg) => tree
      case _ => NamedArg(name, arg).copyAttr(tree)
    }
    def derivedAssign(lhs: Tree[T], rhs: Tree[T]): Assign[T] = tree match {
      case tree: Assign[_] if (lhs eq tree.lhs) && (rhs eq tree.rhs) => tree
      case _ => Assign(lhs, rhs).copyAttr(tree)
    }
    def derivedBlock(stats: List[Tree[T]], expr: Tree[T]): Block[T] = tree match {
      case tree: Block[_] if (stats eq tree.stats) && (expr eq tree.expr) => tree
      case _ => Block(stats, expr).copyAttr(tree)
    }
    def derivedIf(cond: Tree[T], thenp: Tree[T], elsep: Tree[T]): If[T] = tree match {
      case tree: If[_] if (cond eq tree.cond) && (thenp eq tree.thenp) && (elsep eq tree.elsep) => tree
      case _ => If(cond, thenp, elsep).copyAttr(tree)
    }
    def derivedMatch(selector: Tree[T], cases: List[CaseDef[T]]): Match[T] = tree match {
      case tree: Match[_] if (selector eq tree.selector) && (cases eq tree.cases) => tree
      case _ => Match(selector, cases).copyAttr(tree)
    }
    def derivedCaseDef(pat: Tree[T], guard: Tree[T], body: Tree[T]): CaseDef[T] = tree match {
      case tree: CaseDef[_] if (pat eq tree.pat) && (guard eq tree.guard) && (body eq tree.body) => tree
      case _ => CaseDef(pat, guard, body).copyAttr(tree)
    }
    def derivedReturn(expr: Tree[T], from: Ident[T]): Return[T] = tree match {
      case tree: Return[_] if (expr eq tree.expr) && (from eq tree.from) => tree
      case _ => Return(expr, from).copyAttr(tree)
    }
    def derivedTry(block: Tree[T], catches: List[CaseDef[T]], finalizer: Tree[T]): Try[T] = tree match {
      case tree: Try[_] if (block eq tree.block) && (catches eq tree.catches) && (finalizer eq tree.finalizer) => tree
      case _ => Try(block, catches, finalizer).copyAttr(tree)
    }
    def derivedThrow(expr: Tree[T]): Throw[T] = tree match {
      case tree: Throw[_] if (expr eq tree.expr) => tree
      case _ => Throw(expr).copyAttr(tree)
    }
    def derivedSeqLiteral(elemtpt: Tree[T], elems: List[Tree[T]]): SeqLiteral[T] = tree match {
      case tree: SeqLiteral[_] if (elemtpt eq tree.elemtpt) && (elems eq tree.elems) => tree
      case _ => SeqLiteral(elemtpt, elems).copyAttr(tree)
    }
    def derivedTypeTree(original: Tree[T] = EmptyTree[T]): TypeTree[T] = tree match {
      case tree: TypeTree[_] if (original eq tree.original) => tree
      case _ => TypeTree(original).copyAttr(tree)
    }
    def derivedSingletonTypeTree(ref: Tree[T]): SingletonTypeTree[T] = tree match {
      case tree: SingletonTypeTree[_] if (ref eq tree.ref) => tree
      case _ => SingletonTypeTree(ref).copyAttr(tree)
    }
    def derivedSelectFromTypeTree(qualifier: Tree[T], name: Name): SelectFromTypeTree[T] = tree match {
      case tree: SelectFromTypeTree[_] if (qualifier eq tree.qualifier) && (name == tree.name) => tree
      case _ => SelectFromTypeTree(qualifier, name).copyAttr(tree)
    }
    def derivedAndTypeTree(left: Tree[T], right: Tree[T]): AndTypeTree[T] = tree match {
      case tree: AndTypeTree[_] if (left eq tree.left) && (right eq tree.right) => tree
      case _ => AndTypeTree(left, right).copyAttr(tree)
    }
    def derivedOrTypeTree(left: Tree[T], right: Tree[T]): OrTypeTree[T] = tree match {
      case tree: OrTypeTree[_] if (left eq tree.left) && (right eq tree.right) => tree
      case _ => OrTypeTree(left, right).copyAttr(tree)
    }
    def derivedRefineTypeTree(tpt: Tree[T], refinements: List[DefTree[T]]): RefineTypeTree[T] = tree match {
      case tree: RefineTypeTree[_] if (tpt eq tree.tpt) && (refinements eq tree.refinements) => tree
      case _ => RefineTypeTree(tpt, refinements).copyAttr(tree)
    }
    def derivedAppliedTypeTree(tpt: Tree[T], args: List[Tree[T]]): AppliedTypeTree[T] = tree match {
      case tree: AppliedTypeTree[_] if (tpt eq tree.tpt) && (args eq tree.args) => tree
      case _ => AppliedTypeTree(tpt, args).copyAttr(tree)
    }
    def derivedTypeBoundsTree(lo: Tree[T], hi: Tree[T]): TypeBoundsTree[T] = tree match {
      case tree: TypeBoundsTree[_] if (lo eq tree.lo) && (hi eq tree.hi) => tree
      case _ => TypeBoundsTree(lo, hi).copyAttr(tree)
    }
    def derivedBind(name: Name, body: Tree[T]): Bind[T] = tree match {
      case tree: Bind[_] if (name eq tree.name) && (body eq tree.body) => tree
      case _ => Bind(name, body).copyAttr(tree)
    }
    def derivedAlternative(trees: List[Tree[T]]): Alternative[T] = tree match {
      case tree: Alternative[_] if (trees eq tree.trees) => tree
      case _ => Alternative(trees).copyAttr(tree)
    }
    def derivedUnApply(fun: Tree[T], args: List[Tree[T]]): UnApply[T] = tree match {
      case tree: UnApply[_] if (fun eq tree.fun) && (args eq tree.args) => tree
      case _ => UnApply(fun, args).copyAttr(tree)
    }
    def derivedValDef(mods: Modifiers[T], name: TermName, tpt: Tree[T], rhs: Tree[T]): ValDef[T] = tree match {
      case tree: ValDef[_] if (mods == tree.mods) && (name == tree.name) && (tpt eq tree.tpt) && (rhs eq tree.rhs) => tree
      case _ => ValDef(mods, name, tpt, rhs).copyAttr(tree)
    }
    def derivedDefDef(mods: Modifiers[T], name: TermName, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T]): DefDef[T] = tree match {
      case tree: DefDef[_] if (mods == tree.mods) && (name == tree.name) && (tparams eq tree.tparams) && (vparamss eq tree.vparamss) && (tpt eq tree.tpt) && (rhs eq tree.rhs) => tree
      case _ => DefDef(mods, name, tparams, vparamss, tpt, rhs).copyAttr(tree)
    }
    def derivedTypeDef(mods: Modifiers[T], name: TypeName, rhs: Tree[T]): TypeDef[T] = tree match {
      case tree: TypeDef[_] if (mods == tree.mods) && (name == tree.name) && (rhs eq tree.rhs) => tree
      case _ => TypeDef(mods, name, rhs).copyAttr(tree)
    }
    def derivedTemplate(parents: List[Tree[T]], self: ValDef[T], body: List[Tree[T]]): Template[T] = tree match {
      case tree: Template[_] if (parents eq tree.parents) && (self eq tree.self) && (body eq tree.body) => tree
      case _ => Template(parents, self, body).copyAttr(tree)
    }
    def derivedClassDef(mods: Modifiers[T], name: TypeName, tparams: List[TypeDef[T]], impl: Template[T]): ClassDef[T] = tree match {
      case tree: ClassDef[_] if (mods == tree.mods) && (name == tree.name) && (tparams eq tree.tparams) && (impl eq tree.impl) => tree
      case _ => ClassDef(mods, name, tparams, impl).copyAttr(tree)
    }
    def derivedImport(expr: Tree[T], selectors: List[UntypedTree]): Import[T] = tree match {
      case tree: Import[_] if (expr eq tree.expr) && (selectors eq tree.selectors) => tree
      case _ => Import(expr, selectors).copyAttr(tree)
    }
    def derivedPackageDef(pid: RefTree[T], stats: List[Tree[T]]): PackageDef[T] = tree match {
      case tree: PackageDef[_] if (pid eq tree.pid) && (stats eq tree.stats) => tree
      case _ => PackageDef(pid, stats).copyAttr(tree)
    }
    def derivedAnnotated(annot: Tree[T], arg: Tree[T]): Annotated[T] = tree match {
      case tree: Annotated[_] if (annot eq tree.annot) && (arg eq tree.arg) => tree
      case _ => Annotated(annot, arg).copyAttr(tree)
    }
    def derivedShared(shared: Tree[T]): Shared[T] = tree match {
      case tree: Shared[_] if (shared eq tree.shared) => tree
      case _ => Shared(shared).copyAttr(tree)
    }
  }

  abstract class TreeTransformer[T, C] {
    var sharedMemo: Map[Shared[T], Shared[T]] = Map()

    def transform(tree: Tree[T], c: C): Tree[T] = tree match {
      case Ident(name) =>
        finishIdent(tree, tree, c, plugins)
      case Select(qualifier, name) =>
        finishSelect(tree.derivedSelect(transform(qualifier, c), name), tree, c, plugins)
      case This(qual) =>
        finishThis(tree, tree, c, plugins)
      case Super(qual, mix) =>
        finishSuper(tree.derivedSuper(transform(qual, c), mix), tree, c, plugins)
      case Apply(fun, args) =>
        finishApply(tree.derivedApply(transform(fun, c), transform(args, c)), tree, c, plugins)
      case TypeApply(fun, args) =>
        finishTypeApply(tree.derivedTypeApply(transform(fun, c), transform(args, c)), tree, c, plugins)
      case Literal(const) =>
        finishLiteral(tree, tree, c, plugins)
      case New(tpt) =>
        finishNew(tree.derivedNew(transform(tpt, c)), tree, c, plugins)
      case Pair(left, right) =>
        finishPair(tree.derivedPair(transform(left, c), transform(right, c)), tree, c, plugins)
      case Typed(expr, tpt) =>
        finishTyped(tree.derivedTyped(transform(expr, c), transform(tpt, c)), tree, c, plugins)
      case NamedArg(name, arg) =>
        finishNamedArg(tree.derivedNamedArg(name, transform(arg, c)), tree, c, plugins)
      case Assign(lhs, rhs) =>
        finishAssign(tree.derivedAssign(transform(lhs, c), transform(rhs, c)), tree, c, plugins)
      case Block(stats, expr) =>
        finishBlock(tree.derivedBlock(transform(stats, c), transform(expr, c)), tree, c, plugins)
      case If(cond, thenp, elsep) =>
        finishIf(tree.derivedIf(transform(cond, c), transform(thenp, c), transform(elsep, c)), tree, c, plugins)
      case Match(selector, cases) =>
        finishMatch(tree.derivedMatch(transform(selector, c), transformSub(cases, c)), tree, c, plugins)
      case CaseDef(pat, guard, body) =>
        finishCaseDef(tree.derivedCaseDef(transform(pat, c), transform(guard, c), transform(body, c)), tree, c, plugins)
      case Return(expr, from) =>
        finishReturn(tree.derivedReturn(transform(expr, c), transformSub(from, c)), tree, c, plugins)
      case Try(block, catches, finalizer) =>
        finishTry(tree.derivedTry(transform(block, c), transformSub(catches, c), transform(finalizer, c)), tree, c, plugins)
      case Throw(expr) =>
        finishThrow(tree.derivedThrow(transform(expr, c)), tree, c, plugins)
      case SeqLiteral(elemtpt, elems) =>
        finishSeqLiteral(tree.derivedSeqLiteral(transform(elemtpt, c), transform(elems, c)), tree, c, plugins)
      case TypeTree(original) =>
        finishTypeTree(tree.derivedTypeTree(transform(original, c)), tree, c, plugins)
      case SingletonTypeTree(ref) =>
        finishSingletonTypeTree(tree.derivedSingletonTypeTree(transform(ref, c)), tree, c, plugins)
      case SelectFromTypeTree(qualifier, name) =>
        finishSelectFromTypeTree(tree.derivedSelectFromTypeTree(transform(qualifier, c), name), tree, c, plugins)
      case AndTypeTree(left, right) =>
        finishAndTypeTree(tree.derivedAndTypeTree(transform(left, c), transform(right, c)), tree, c, plugins)
      case OrTypeTree(left, right) =>
        finishOrTypeTree(tree.derivedOrTypeTree(transform(left, c), transform(right, c)), tree, c, plugins)
      case RefineTypeTree(tpt, refinements) =>
        finishRefineTypeTree(tree.derivedRefineTypeTree(transform(tpt, c), transformSub(refinements, c)), tree, c, plugins)
      case AppliedTypeTree(tpt, args) =>
        finishAppliedTypeTree(tree.derivedAppliedTypeTree(transform(tpt, c), transform(args, c)), tree, c, plugins)
      case TypeBoundsTree(lo, hi) =>
        finishTypeBoundsTree(tree.derivedTypeBoundsTree(transform(lo, c), transform(hi, c)), tree, c, plugins)
      case Bind(name, body) =>
        finishBind(tree.derivedBind(name, transform(body, c)), tree, c, plugins)
      case Alternative(trees) =>
        finishAlternative(tree.derivedAlternative(transform(trees, c)), tree, c, plugins)
      case UnApply(fun, args) =>
        finishUnApply(tree.derivedUnApply(transform(fun, c), transform(args, c)), tree, c, plugins)
      case ValDef(mods, name, tpt, rhs) =>
        finishValDef(tree.derivedValDef(mods, name, transform(tpt, c), transform(rhs, c)), tree, c, plugins)
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        finishDefDef(tree.derivedDefDef(mods, name, transformSub(tparams, c), vparamss mapConserve (transformSub(_, c)), transform(tpt, c), transform(rhs, c)), tree, c, plugins)
      case TypeDef(mods, name, rhs) =>
        finishTypeDef(tree.derivedTypeDef(mods, name, transform(rhs, c)), tree, c, plugins)
      case Template(parents, self, body) =>
        finishTemplate(tree.derivedTemplate(transform(parents, c), transformSub(self, c), transform(body, c)), tree, c, plugins)
      case ClassDef(mods, name, tparams, impl) =>
        finishClassDef(tree.derivedClassDef(mods, name, transformSub(tparams, c), transformSub(impl, c)), tree, c, plugins)
      case Import(expr, selectors) =>
        finishImport(tree.derivedImport(transform(expr, c), selectors), tree, c, plugins)
      case PackageDef(pid, stats) =>
        finishPackageDef(tree.derivedPackageDef(transformSub(pid, c), transform(stats, c)), tree, c, plugins)
      case Annotated(annot, arg) =>
        finishAnnotated(tree.derivedAnnotated(transform(annot, c), transform(arg, c)), tree, c, plugins)
      case EmptyTree() =>
        finishEmptyTree(tree, tree, c, plugins)
      case tree @ Shared(shared) =>
        finishShared(
          sharedMemo get tree match {
          case Some(tree1) => tree1
          case None =>
            val tree1 = tree.derivedShared(transform(shared, c))
            sharedMemo = sharedMemo.updated(tree, tree1)
            tree1
        },
        tree, c, plugins)
    }
    def transform(trees: List[Tree[T]], c: C): List[Tree[T]] =
      trees mapConserve (transform(_, c))
    def transformSub(tree: Tree[T], c: C): tree.ThisTree[T] =
      transform(tree, c).asInstanceOf[tree.ThisTree[T]]
    def transformSub[TT <: Tree[T]](trees: List[TT], c: C): List[TT] =
      transform(trees, c).asInstanceOf[List[TT]]

    type Plugins >: Null
    def plugins: Plugins = null

    def finishIdent(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishSelect(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishThis(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishSuper(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishApply(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishTypeApply(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishLiteral(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishNew(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishPair(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishTyped(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishNamedArg(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishAssign(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishFunction(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishBlock(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishIf(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishMatch(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishCaseDef(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishReturn(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishTry(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishThrow(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishSeqLiteral(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishTypeTree(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishSingletonTypeTree(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishSelectFromTypeTree(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishAndTypeTree(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishOrTypeTree(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishRefineTypeTree(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishAppliedTypeTree(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishTypeBoundsTree(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishBind(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishAlternative(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishUnApply(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishValDef(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishDefDef(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishTypeDef(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishTemplate(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishClassDef(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishImport(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishPackageDef(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishAnnotated(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishEmptyTree(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishShared(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
  }

  abstract class TreeAccumulator[T, U] extends ((T, Tree[U]) => T) {
    var sharedMemo: Map[Shared[U], T] = Map()
    def apply(x: T, tree: Tree[U]): T
    def apply(x: T, trees: List[Tree[U]]): T = (x /: trees)(apply)
    def foldOver(x: T, tree: Tree[U]): T = tree match {
      case Ident(name) =>
        x
      case Select(qualifier, name) =>
        this(x, qualifier)
      case This(qual) =>
        x
      case Super(qual, mix) =>
        this(x, qual)
      case Apply(fun, args) =>
        this(this(x, fun), args)
      case TypeApply(fun, args) =>
        this(this(x, fun), args)
      case Literal(const) =>
        x
      case New(tpt) =>
        this(x, tpt)
      case Pair(left, right) =>
        this(this(x, left), right)
      case Typed(expr, tpt) =>
        this(this(x, expr), tpt)
      case NamedArg(name, arg) =>
        this(x, arg)
      case Assign(lhs, rhs) =>
        this(this(x, lhs), rhs)
      case Block(stats, expr) =>
        this(this(x, stats), expr)
      case If(cond, thenp, elsep) =>
        this(this(this(x, cond), thenp), elsep)
      case Match(selector, cases) =>
        this(this(x, selector), cases)
      case CaseDef(pat, guard, body) =>
        this(this(this(x, pat), guard), body)
      case Return(expr, from) =>
        this(this(x, expr), from)
      case Try(block, catches, finalizer) =>
        this(this(this(x, block), catches), finalizer)
      case Throw(expr) =>
        this(x, expr)
      case SeqLiteral(elemtpt, elems) =>
        this(this(x, elemtpt), elems)
      case TypeTree(original) =>
        x
      case SingletonTypeTree(ref) =>
        this(x, ref)
      case SelectFromTypeTree(qualifier, name) =>
        this(x, qualifier)
      case AndTypeTree(left, right) =>
        this(this(x, left), right)
      case OrTypeTree(left, right) =>
        this(this(x, left), right)
      case RefineTypeTree(tpt, refinements) =>
        this(this(x, tpt), refinements)
      case AppliedTypeTree(tpt, args) =>
        this(this(x, tpt), args)
      case TypeBoundsTree(lo, hi) =>
        this(this(x, lo), hi)
      case Bind(name, body) =>
        this(x, body)
      case Alternative(trees) =>
        this(x, trees)
      case UnApply(fun, args) =>
        this(this(x, fun), args)
      case ValDef(mods, name, tpt, rhs) =>
        this(this(x, tpt), rhs)
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        this(this((this(x, tparams) /: vparamss)(apply), tpt), rhs)
      case TypeDef(mods, name, rhs) =>
        this(x, rhs)
      case Template(parents, self, body) =>
        this(this(this(x, parents), self), body)
      case ClassDef(mods, name, tparams, impl) =>
        this(this(x, tparams), impl)
      case Import(expr, selectors) =>
        this(x, expr)
      case PackageDef(pid, stats) =>
        this(this(x, pid), stats)
      case Annotated(annot, arg) =>
        this(this(x, annot), arg)
      case EmptyTree() =>
        x
      case tree @ Shared(shared) =>
        sharedMemo get tree match {
          case Some(x1) => x1
          case None =>
            val x1 = this(x, shared)
            sharedMemo = sharedMemo.updated(tree, x1)
            x1
        }
    }
  }

  /** Fold `f` over all tree nodes, in depth-first, prefix order */
  class DeepFolder[T, U](f: (T, Tree[U]) => T) extends TreeAccumulator[T, U] {
    def apply(x: T, tree: Tree[U]): T = foldOver(f(x, tree), tree)
  }

  /** Fold `f` over all tree nodes, in depth-first, prefix order, but don't visit
   *  subtrees where `f` returns a different result for the root, i.e. `f(x, root) ne x`.
   */
  class ShallowFolder[T, U](f: (T, Tree[U]) => T) extends TreeAccumulator[T, U] {
    def apply(x: T, tree: Tree[U]): T = {
      val x1 = f(x, tree)
      if (x1.asInstanceOf[AnyRef] ne x1.asInstanceOf[AnyRef]) x1
      else foldOver(x1, tree)
    }
  }
}
