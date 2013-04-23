package dotty.tools.dotc
package core

import Types._, Names._, Flags._, util.Positions._, Contexts._, Constants._, SymDenotations._, Symbols._
import Denotations._, StdNames._
import annotation.tailrec
import language.higherKinds
import collection.mutable
import collection.mutable.ArrayBuffer

object Trees {

  // Note: it would be more logical to make Untyped = Nothing.
  // However, this interacts in a bad way with Scala's current type inference.
  // In fact, we cannot write soemthing like Select(pre, name), where pre is
  // of type Tree[Nothing]; type inference will treat the Nothing as an uninstantited
  // value and will not infer Nothing as the type parameter for Select.
  // We should come back to this issue once type inference is changed.
  type Untyped = Null
  type TypedTree = Tree[Type]
  type UntypedTree = Tree[Untyped]

  /** Modifiers and annotations for definitions
   *  @param flags          The set flags
   *  @param privateWithin  If a private or protected has is followed by a
   *                        qualifier [q], the name q, "" as a typename otherwise.
   *  @param annotations    The annotations preceding the modifers
   *  @param positions      A flagPositions structure that records the positions
   *                        of et flags.
   *  @param pos            The position of the modifiers. This should start with
   *                        the first modifier or annotation and have as point
   *                        the start of the opening keyword(s) of the definition.
   *                        It should have as end the end of the opening keywords(s).
   */
  case class Modifiers[T >: Untyped](
    flags: FlagSet = EmptyFlags,
    privateWithin: TypeName = tpnme.EMPTY,
    annotations: List[Tree[T]] = Nil,
    positions: FlagPositions = NoFlagPositions)(implicit val cpos: Position = NoPosition) {

    def | (fs: FlagSet): Modifiers[T] = copy(flags = flags | fs)(cpos)
    def & (fs: FlagSet): Modifiers[T] = copy(flags = flags & fs)(cpos)
    def &~(fs: FlagSet): Modifiers[T] = copy(flags = flags &~ fs)(cpos)

    def is(fs: FlagSet): Boolean = flags is fs
    def is(fc: FlagConjunction): Boolean = flags is fc

    def add(flag: FlagSet, start: Int) =
      copy(flags = flags | flag, positions = positions.add(flag, start))

    def withAnnotations(annots: List[Tree[T]]) =
      if (annots.isEmpty) this
      else copy(annotations = annotations ++ annots)(cpos)

    def withPrivateWithin(pw: TypeName) =
      if (pw.isEmpty) this
      else copy(privateWithin = pw)(cpos)

    def withPos(pos: Position) = copy()(pos)

    val pos: Position = unionPos(cpos, annotations)
  }

  private final val OffsetShift = 6
  private final val FlagMask = (1 << OffsetShift) - 1

  class FlagPositions(val arr: Array[Int]) extends AnyVal {
    def get(flag: FlagSet): Position = {
      val str :: Nil = flag.flagStrings.toList
      val code = flag.firstBit
      var i = 0
      while (i < arr.length && ((arr(i) & FlagMask) != code)) i += 1
      if (i < arr.length) {
        val start = arr(i) >>> OffsetShift
        val end = start + str.length
        Position(start, end)
      } else
        NoPosition
    }
    def add(flag: FlagSet, start: Int) = {
      val newarr = new Array[Int](arr.length + 1)
      arr.copyToArray(newarr)
      setFlagPosition(newarr, arr.length, flag, start)
      new FlagPositions(newarr)
    }
  }

  private def setFlagPosition(arr: Array[Int], idx: Int, flag: FlagSet, start: Int): Unit = {
    val code = flag.firstBit
    assert(code <= 64)
    arr(idx) = code | (start << OffsetShift)
  }

  def FlagPositions(assocs: ArrayBuffer[(FlagSet, Int)]): FlagPositions = {
    val arr = Array[Int](assocs.size)
    for (i <- 0 until assocs.size) {
      val (flag, start) = assocs(i)
      setFlagPosition(arr, i, flag, start)
    }
    FlagPositions(assocs)
  }

  val NoFlagPositions = new FlagPositions(Array())

  /** Trees take a parameter indicating what the type of their `tpe` field
   *  is. Two choices: `Type` or `Untyped`.
   *  Untyped trees have type `Tree[Untyped]`.
   *
   *  Tree typing uses a copy-on-write implementation:
   *
   *   - You can never observe a `tpe` which is `null` (throws an exception)
   *   - So when creating a typed tree with `withType` we can re-use
   *     the existing tree transparently, assigning its `tpe` field,
   *     provided it was `null` before.
   *   - It is impossible to embed untyped trees in typed ones.
   *   - Typed trees can be embedded untyped ones provided they are rooted
   *     in a TypedSplice node.
   *   - Type checking an untyped tree should remove all embedded `TypedSplice`
   *     nodes.
   */
  abstract class Tree[T >: Untyped] extends DotClass with Showable with Cloneable {

    /** The tree's position. Except
     *  for SharedTree nodes, it is always ensured that a tree's position
     *  contains the positions of all its immediate subtrees. However, it need not
     *  contain positions of other tree elements such as Modifiers.
     */
    def pos: Position

    /** The extended position. Unlike `pos`, this one contains positions of modifiers */
    def xpos: Position = pos

    /** The type  constructor at the root of the tree */
    type ThisTree[T >: Untyped] <: Tree[T]

    private var _tpe: T = _

    /** The type of the tree. In case of an untyped tree,
     *   an UnAssignedTypeException is thrown. (Overridden by empty trees)
     */
    def tpe: T = {
      if (_tpe == null) throw new UnAssignedTypeException(this)
      _tpe
    }

    /** Copy `tpe` attribute from tree `from` into this tree, independently
     *  whether it is null or not.
     */
    final def copyAttr(from: Tree[T]): ThisTree[T] = {
      _tpe = from._tpe
      this.asInstanceOf[ThisTree[T]]
    }

    /** Return a typed tree that's isomorphic to this tree, but has given
     *  type. (Overridden by empty trees)
     */
    def withType(tpe: Type): ThisTree[Type] = {
      val tree =
        (if (_tpe == null ||
          (_tpe.asInstanceOf[AnyRef] eq tpe.asInstanceOf[AnyRef])) this
        else clone).asInstanceOf[Tree[Type]]
      tree._tpe = tpe
      tree.asInstanceOf[ThisTree[Type]]
    }

    /** Does the tree have its type field set? Note: this operation is not
     *  referentially transparent, because it can observe the withType
     *  modifications. Should be used only in special circumstances (we
     *  need it for printing trees with optional type info).
     */
    final def hasType: Boolean = _tpe != null

    final def typeOpt: Type = _tpe match {
      case tp: Type => tp
      case _ => NoType
    }

    /** The denotation referred to by this tree.
     *  Defined for `DenotingTree`s and `ProxyTree`s, NoDenotation for other
     *  kinds of trees
     */
    def denot(implicit ctx: Context): Denotation = NoDenotation

    /** Shorthand for `denot.symbol`. */
    final def symbol(implicit ctx: Context): Symbol = denot.symbol

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

    /** if this tree is the empty tree, the alternative, else this tree */
    def orElse(that: => Tree[T]): Tree[T] =
      if (this eq theEmptyTree) that else this

    override def toText(implicit ctx: Context) = ctx.toText(this)

    override def hashCode(): Int = System.identityHashCode(this)
    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
  }

  class UnAssignedTypeException[T >: Untyped](tree: Tree[T]) extends RuntimeException {
    override def getMessage: String = s"type of $tree is not assigned"
  }

  // ------ Categories of trees -----------------------------------

  /** Instances of this class are trees for which isType is definitely true.
   *  Note that some trees have isType = true without being TypTrees (e.g. Ident, AnnotatedTree)
   */
  trait TypTree[T >: Untyped] extends Tree[T] {
    type ThisTree[T >: Untyped] <: TypTree[T]
    override def isType = true
  }

  /** Instances of this class are trees for which isTerm is definitely true.
   *  Note that some trees have isTerm = true without being TermTrees (e.g. Ident, AnnotatedTree)
   */
  trait TermTree[T >: Untyped] extends Tree[T] {
    type ThisTree[T >: Untyped] <: TermTree[T]
    override def isTerm = true
  }

  /** Instances of this class are trees which are not terms but are legal
   *  parts of patterns.
   */
  trait PatternTree[T >: Untyped] extends Tree[T] {
    type ThisTree[T >: Untyped] <: PatternTree[T]
    override def isPattern = true
  }

  /** Tree's denotation can be derived from its type */
  abstract class DenotingTree[T >: Untyped] extends Tree[T] {
    type ThisTree[T >: Untyped] <: DenotingTree[T]
    override def denot(implicit ctx: Context) = tpe match {
      case tpe: NamedType => tpe.denot
      case _ => NoDenotation
    }
  }

  /** Tree's denot/isType/isTerm properties come from a subtree
   *  identified by `forwardTo`.
   */
  abstract class ProxyTree[T >: Untyped] extends Tree[T] {
    type ThisTree[T >: Untyped] <: ProxyTree[T]
    def forwardTo: Tree[T]
    override def denot(implicit ctx: Context): Denotation = forwardTo.denot
    override def isTerm = forwardTo.isTerm
    override def isType = forwardTo.isType
  }

  /** Tree has a name */
  abstract class NameTree[T >: Untyped] extends DenotingTree[T] {
    type ThisTree[T >: Untyped] <: NameTree[T]
    def name: Name
  }

  /** Tree refers by name to a denotation */
  abstract class RefTree[T >: Untyped] extends NameTree[T] {
    type ThisTree[T >: Untyped] <: RefTree[T]
    def qualifier: Tree[T]
    override def isType = name.isTypeName
    override def isTerm = name.isTermName
  }

  /** Tree defines a new symbol */
  trait DefTree[T >: Untyped] extends DenotingTree[T] {
    type ThisTree[T >: Untyped] <: DefTree[T]
    override def isDef = true
  }

  /** Tree defines a new symbol and carries modifiers.
   *  The position of a ModDefTree usually starts
   *  at the symbol that's being defined (known exception:
   *  A type definition starts at the +/- if there is one).
   *  The annotations, modifiers and keyword that come before
   *  are subsumed in the modifier position.
   */
  trait ModDefTree[T >: Untyped] extends DefTree[T] {
    type ThisTree[T >: Untyped] <: ModDefTree[T]
    def mods: Modifiers[T]
    override def xpos = mods.pos union pos
  }

  // ----------- Tree case classes ------------------------------------

  /** name */
  case class Ident[T >: Untyped](name: Name)(implicit cpos: Position)
    extends RefTree[T] {
    type ThisTree[T >: Untyped] = Ident[T]
    val pos = cpos
    def qualifier: Tree[T] = EmptyTree[T]
  }

  class BackquotedIdent[T >: Untyped](name: Name)(implicit cpos: Position)
    extends Ident[T](name)

  /** qualifier.name */
  case class Select[T >: Untyped](qualifier: Tree[T], name: Name)(implicit cpos: Position)
    extends RefTree[T] {
    type ThisTree[T >: Untyped] = Select[T]
    val pos = cpos union qualifier.pos
  }

  /** qual.this */
  case class This[T >: Untyped](qual: TypeName)(implicit cpos: Position)
    extends DenotingTree[T] with TermTree[T] {
    type ThisTree[T >: Untyped] = This[T]
    val pos = cpos
  }

  /** C.super[mix], where qual = C.this */
  case class Super[T >: Untyped](qual: Tree[T], mix: TypeName)(implicit cpos: Position)
    extends ProxyTree[T] with TermTree[T] {
    type ThisTree[T >: Untyped] = Super[T]
    val pos = cpos union qual.pos
    def forwardTo = qual
  }

  abstract class GenericApply[T >: Untyped] extends ProxyTree[T] with TermTree[T] {
    type ThisTree[T >: Untyped] <: GenericApply[T]
    val fun: Tree[T]
    val args: List[Tree[T]]
    def forwardTo = fun
  }

  /** fun(args) */
  case class Apply[T >: Untyped](fun: Tree[T], args: List[Tree[T]])(implicit cpos: Position)
    extends GenericApply[T] {
    type ThisTree[T >: Untyped] = Apply[T]
    val pos = unionPos(cpos union fun.pos, args)
  }

  /** fun[args] */
  case class TypeApply[T >: Untyped](fun: Tree[T], args: List[Tree[T]])(implicit cpos: Position)
    extends GenericApply[T] {
    type ThisTree[T >: Untyped] = TypeApply[T]
    val pos = unionPos(cpos union fun.pos, args)
  }

  /** const */
  case class Literal[T >: Untyped](const: Constant)(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T >: Untyped] = Literal[T]
    val pos = cpos
  }

  /** new tpt, but no constructor call */
  case class New[T >: Untyped](tpt: Tree[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T >: Untyped] = New[T]
    val pos = cpos union tpt.pos
  }

  /** new tpt(args1)...(args_n)
   *  @param cpos A position spanning the new.
   */
  def New[T >: Untyped](tpt: Tree[T], argss: List[List[Tree[T]]])(implicit cpos: Position): Tree[T] = {
    val newPos = cpos.withEnd(tpt.pos.end min cpos.end)
    locally {
      implicit val cpos: Position = newPos
      ((Select(New(tpt), nme.CONSTRUCTOR): Tree[T]) /: argss)(Apply(_, _))
    }
  }

  /** (left, right) */
  case class Pair[T >: Untyped](left: Tree[T], right: Tree[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T >: Untyped] = Pair[T]
    val pos = cpos union left.pos union right.pos
    override def isTerm = left.isTerm && right.isTerm
    override def isType = left.isType && right.isType
    override def isPattern = !isTerm && (left.isPattern || left.isTerm) && (right.isPattern || right.isTerm)
  }

  /** expr : tpt */
  case class Typed[T >: Untyped](expr: Tree[T], tpt: Tree[T])(implicit cpos: Position)
    extends ProxyTree[T] with TermTree[T] {
    type ThisTree[T >: Untyped] = Typed[T]
    val pos = cpos union expr.pos union tpt.pos
    def forwardTo = expr
  }

  /** name = arg, in a parameter list */
  case class NamedArg[T >: Untyped](name: Name, arg: Tree[T])(implicit cpos: Position)
    extends Tree[T] {
    type ThisTree[T >: Untyped] = NamedArg[T]
    val pos = cpos union arg.pos
  }

  /** name = arg, outside a parameter list */
  case class Assign[T >: Untyped](lhs: Tree[T], rhs: Tree[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T >: Untyped] = Assign[T]
    val pos = cpos union lhs.pos union rhs.pos
  }

  /** { stats; expr } */
  case class Block[T >: Untyped](stats: List[Tree[T]], expr: Tree[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T >: Untyped] = Block[T]
    val pos = unionPos(cpos union expr.pos, stats)
  }

  /** if cond then thenp else elsep */
  case class If[T >: Untyped](cond: Tree[T], thenp: Tree[T], elsep: Tree[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T >: Untyped] = If[T]
    val pos = cpos union cond.pos union thenp.pos union elsep.pos
  }

  /** selector match { cases } */
  case class Match[T >: Untyped](selector: Tree[T], cases: List[CaseDef[T]])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T >: Untyped] = Match[T]
    val pos = unionPos(cpos union selector.pos, cases)
  }

  /** case pat if guard => body */
  case class CaseDef[T >: Untyped](pat: Tree[T], guard: Tree[T], body: Tree[T])(implicit cpos: Position)
    extends Tree[T] {
    type ThisTree[T >: Untyped] = CaseDef[T]
    val pos = cpos union pat.pos union guard.pos union body.pos
  }

  /** return expr
   *  where `from` refers to the method from which the return takes place
   *  After program transformations this is not necessarily the enclosing method, because
   *  closures can intervene.
   */
  case class Return[T >: Untyped](expr: Tree[T], from: Tree[T] = EmptyTree[T]())(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T >: Untyped] = Return[T]
    val pos = cpos union expr.pos // from is synthetic, does not influence pos
  }

  /** try block catch { catches } */
  case class Try[T >: Untyped](block: Tree[T], catches: List[CaseDef[T]], finalizer: Tree[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T >: Untyped] = Try[T]
    val pos = unionPos(cpos union block.pos union finalizer.pos, catches)
  }

  /** throw expr */
  case class Throw[T >: Untyped](expr: Tree[T])(implicit cpos: Position)
    extends TermTree[T] {
    type ThisTree[T >: Untyped] = Throw[T]
    val pos = cpos union expr.pos
  }

  /** Array[elemtpt](elems) */
  case class SeqLiteral[T >: Untyped](elemtpt: Tree[T], elems: List[Tree[T]])(implicit cpos: Position)
    extends Tree[T] {
    type ThisTree[T >: Untyped] = SeqLiteral[T]
    val pos = unionPos(cpos union elemtpt.pos, elems)
  }

  /** A type tree that represents an existing or inferred type */
  case class TypeTree[T >: Untyped](original: Tree[T] = EmptyTree[T])(implicit cpos: Position)
    extends DenotingTree[T] with TypTree[T] {
    type ThisTree[T >: Untyped] = TypeTree[T]
    val pos = if (original.pos.exists) original.pos else cpos.focus
  }

  /** ref.type */
  case class SingletonTypeTree[T >: Untyped](ref: Tree[T])(implicit cpos: Position)
    extends DenotingTree[T] with TypTree[T] {
    type ThisTree[T >: Untyped] = SingletonTypeTree[T]
    val pos = cpos union ref.pos
  }

  /** qualifier # name */
  case class SelectFromTypeTree[T >: Untyped](qualifier: Tree[T], name: Name)(implicit cpos: Position)
    extends RefTree[T] {
    type ThisTree[T >: Untyped] = SelectFromTypeTree[T]
    val pos = cpos union qualifier.pos
  }

  /** left & right */
  case class AndTypeTree[T >: Untyped](left: Tree[T], right: Tree[T])(implicit cpos: Position)
    extends TypTree[T] {
    type ThisTree[T >: Untyped] = AndTypeTree[T]
    val pos = cpos union left.pos union right.pos
  }

  /** left | right */
  case class OrTypeTree[T >: Untyped](left: Tree[T], right: Tree[T])(implicit cpos: Position)
    extends TypTree[T] {
    type ThisTree[T >: Untyped] = OrTypeTree[T]
    val pos = cpos union left.pos union right.pos
  }

  /** tpt { refinements } */
  case class RefineTypeTree[T >: Untyped](tpt: Tree[T], refinements: List[Tree[T]])(implicit cpos: Position)
    extends ProxyTree[T] with TypTree[T] {
    type ThisTree[T >: Untyped] = RefineTypeTree[T]
    val pos = unionPos(cpos union tpt.pos, refinements)
    def forwardTo = tpt
  }

  /** tpt[args] */
  case class AppliedTypeTree[T >: Untyped](tpt: Tree[T], args: List[Tree[T]])(implicit cpos: Position)
    extends ProxyTree[T] with TypTree[T] {
    type ThisTree[T >: Untyped] = AppliedTypeTree[T]
    val pos = unionPos(cpos union tpt.pos, args)
    def forwardTo = tpt
  }

  def AppliedTypeTree[T >: Untyped](tpt: Tree[T], arg: Tree[T])(implicit cpos: Position): AppliedTypeTree[T] =
    AppliedTypeTree(tpt, arg :: Nil)

  /** >: lo <: hi */
  case class TypeBoundsTree[T >: Untyped](lo: Tree[T], hi: Tree[T])(implicit cpos: Position)
    extends Tree[T] {
    type ThisTree[T >: Untyped] = TypeBoundsTree[T]
    val pos = cpos union lo.pos union hi.pos
  }

  /** name @ body */
  case class Bind[T >: Untyped](name: Name, body: Tree[T])(implicit cpos: Position)
    extends NameTree[T] with DefTree[T] with PatternTree[T] {
    type ThisTree[T >: Untyped] = Bind[T]
    val pos = cpos union body.pos
  }

  /** tree_1 | ... | tree_n */
  case class Alternative[T >: Untyped](trees: List[Tree[T]])(implicit cpos: Position)
    extends PatternTree[T] {
    type ThisTree[T >: Untyped] = Alternative[T]
    val pos = unionPos(cpos, trees)
  }

  /** fun(args) in a pattern, if fun is an extractor */
  case class UnApply[T >: Untyped](fun: Tree[T], args: List[Tree[T]])(implicit cpos: Position)
    extends PatternTree[T] {
    type ThisTree[T >: Untyped] = UnApply[T]
    val pos = unionPos(cpos union fun.pos, args)
  }

  /** mods val name: tpt = rhs */
  case class ValDef[T >: Untyped](mods: Modifiers[T], name: TermName, tpt: Tree[T], rhs: Tree[T])(implicit cpos: Position)
    extends NameTree[T] with ModDefTree[T] {
    type ThisTree[T >: Untyped] = ValDef[T]
    val pos = cpos union tpt.pos union rhs.pos
  }

  /** mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs */
  case class DefDef[T >: Untyped](mods: Modifiers[T], name: TermName, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T])(implicit cpos: Position)
    extends NameTree[T] with ModDefTree[T] {
    type ThisTree[T >: Untyped] = DefDef[T]
    val pos = (unionPos(cpos union tpt.pos union rhs.pos, tparams) /: vparamss)(unionPos)
  }

  class ImplicitDefDef[T >: Untyped](mods: Modifiers[T], name: TermName, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T])(implicit pos: Position) extends DefDef[T](mods, name, tparams, vparamss, tpt, rhs) {
    override def copy[T >: Untyped](mods: Modifiers[T], name: TermName, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T])(implicit pos: Position) =
      new ImplicitDefDef[T](mods, name, tparams, vparamss, tpt, rhs)
  }

  /** mods type name = rhs   or
   *  mods type name >: lo <: hi, if rhs = TypeBoundsTree(lo, hi)
   */
  case class TypeDef[T >: Untyped](mods: Modifiers[T], name: TypeName, tparams: List[TypeDef[T]], rhs: Tree[T])(implicit cpos: Position)
    extends NameTree[T] with ModDefTree[T] {
    type ThisTree[T >: Untyped] = TypeDef[T]
    val pos = unionPos(cpos union rhs.pos, tparams)
  }

  /** extends parents { self => body } */
  case class Template[T >: Untyped](parents: List[Tree[T]], self: ValDef[T], body: List[Tree[T]])(implicit cpos: Position)
    extends DefTree[T] {
    type ThisTree[T >: Untyped] = Template[T]
    val pos = unionPos(unionPos(cpos union self.xpos, parents), body)
  }

  /** mods class name[tparams] impl */
  case class ClassDef[T >: Untyped](mods: Modifiers[T], name: TypeName, tparams: List[TypeDef[T]], impl: Template[T])(implicit cpos: Position)
    extends NameTree[T] with ModDefTree[T] {
    type ThisTree[T >: Untyped] = ClassDef[T]
    val pos = unionPos(cpos union impl.pos, tparams)
  }

  /** import expr.selectors
   *  where a selector is either an untyped `Ident`, `name` or
   *  an untyped `Pair` `name => rename`
   */
  case class Import[T >: Untyped](expr: Tree[T], selectors: List[UntypedTree])(implicit cpos: Position)
    extends DenotingTree[T] {
    type ThisTree[T >: Untyped] = Import[T]
    val pos = unionPos(cpos union expr.pos, selectors)
  }

  /** package pid { stats } */
  case class PackageDef[T >: Untyped](pid: RefTree[T], stats: List[Tree[T]])(implicit cpos: Position)
    extends ProxyTree[T] {
    type ThisTree[T >: Untyped] = PackageDef[T]
    val pos = unionPos(cpos union pid.pos, stats)
    def forwardTo = pid
  }

  /** arg @annot */
  case class Annotated[T >: Untyped](annot: Tree[T], arg: Tree[T])(implicit cpos: Position)
    extends ProxyTree[T] {
    type ThisTree[T >: Untyped] = Annotated[T]
    val pos = cpos union annot.pos union arg.pos
    def forwardTo = arg
  }

  trait AlwaysEmpty[T >: Untyped] extends Tree[T] {
    override val pos = NoPosition
    override def tpe = unsupported("tpe")
    override def withType(tpe: Type) = unsupported("withType")
    override def isEmpty: Boolean = true
  }

  /** A missing tree */
  abstract case class EmptyTree[T >: Untyped]()
    extends Tree[T] with AlwaysEmpty[T] {
    type ThisTree[T >: Untyped] = EmptyTree[T]
  }

  private object theEmptyTree extends EmptyTree[Untyped]

  object EmptyTree {
    def apply[T >: Untyped](): EmptyTree[T] = theEmptyTree.asInstanceOf[EmptyTree[T]]
  }

  class EmptyValDef[T >: Untyped] extends ValDef[T](
    Modifiers[T](Private)(NoPosition), nme.WILDCARD, EmptyTree[T], EmptyTree[T])(NoPosition) with AlwaysEmpty[T]

  private object theEmptyValDef extends EmptyValDef[Untyped]

  object EmptyValDef {
    def apply[T >: Untyped](): EmptyValDef[T] = theEmptyValDef.asInstanceOf[EmptyValDef[T]]
  }

  /** A tree that can be shared without its position
   *  polluting containing trees. Accumulators and tranformers
   *  memoize results of shared subtrees
   */
  case class SharedTree[T >: Untyped](shared: Tree[T]) extends ProxyTree[T] {
    type ThisTree[T >: Untyped] = SharedTree[T]
    def forwardTo: Tree[T] = shared
    val pos = NoPosition
  }

  // ----- Tree cases that exist in untyped form only ------------------

  /** A typed subtree of an untyped tree needs to be wrapped in a TypedSlice */
  case class TypedSplice(tree: TypedTree) extends UntypedTree {
    val pos = tree.pos
  }

  /** mods object name impl */
  case class ModuleDef(mods: Modifiers[Untyped], name: TermName, impl: Template[Untyped])(implicit cpos: Position)
    extends NameTree[Untyped] with ModDefTree[Untyped] {
    type ThisTree[T >: Untyped] <: NameTree[T] with ModDefTree[T] with ModuleDef
    val pos = cpos union impl.pos
    def derivedModuleDef(mods: Modifiers[Untyped], name: TermName, impl: Template[Untyped]) =
      if (mods == this.mods && name == this.name && (impl eq this.impl)) this
      else ModuleDef(mods, name, impl)
  }

  /** (vparams) => body */
  case class Function(vparams: List[ValDef[Untyped]], body: Tree[Untyped])(implicit cpos: Position)
    extends TermTree[Untyped] {
    type ThisTree[T >: Untyped] <: TermTree[T] with Function
    val pos = unionPos(cpos union body.pos, vparams)
  }

  /** Something in parentheses */
  case class Parens(trees: List[Tree[Untyped]])(implicit cpos: Position) extends Tree[Untyped] {
    type ThisTree[T >: Untyped] <: Tree[T] with Parens
    val pos = unionPos(cpos, trees)
  }

  // ----- Auxiliary creation methods ------------------

  def Block[T >: Untyped](stat: Tree[T], expr: Tree[T])(implicit cpos: Position): Block[T] =
    Block(stat :: Nil, expr)

  def Apply[T >: Untyped](fn: Tree[T], arg: Tree[T])(implicit cpos: Position): Apply[T] =
    Apply(fn, arg :: Nil)

  def Function(vparam: ValDef[Untyped], body: Tree[Untyped])(implicit cpos: Position): Function =
    Function(vparam :: Nil, body)

  // ----- Generic Tree Instances, inherited from  `tpt` and `untpd`.

  abstract class Instance[T >: Untyped] {

    type Modifiers = Trees.Modifiers[T]
    type Tree = Trees.Tree[T]
    type TypTree = Trees.TypTree[T]
    type TermTree = Trees.TermTree[T]
    type PatternTree = Trees.PatternTree[T]
    type DenotingTree = Trees.DenotingTree[T]
    type ProxyTree = Trees.ProxyTree[T]
    type NameTree = Trees.NameTree[T]
    type RefTree = Trees.RefTree[T]
    type DefTree = Trees.DefTree[T]

    type TreeCopier = Trees.TreeCopier[T]
    type TreeAccumulator[U] = Trees.TreeAccumulator[U, T]
    type TreeTransformer = Trees.TreeTransformer[T]

    type Ident = Trees.Ident[T]
    type Select = Trees.Select[T]
    type This = Trees.This[T]
    type Super = Trees.Super[T]
    type Apply = Trees.Apply[T]
    type TypeApply = Trees.TypeApply[T]
    type Literal = Trees.Literal[T]
    type New = Trees.New[T]
    type Pair = Trees.Pair[T]
    type Typed = Trees.Typed[T]
    type NamedArg = Trees.NamedArg[T]
    type Assign = Trees.Assign[T]
    type Block = Trees.Block[T]
    type If = Trees.If[T]
    type Match = Trees.Match[T]
    type CaseDef = Trees.CaseDef[T]
    type Return = Trees.Return[T]
    type Try = Trees.Try[T]
    type Throw = Trees.Throw[T]
    type SeqLiteral = Trees.SeqLiteral[T]
    type TypeTree = Trees.TypeTree[T]
    type SingletonTypeTree = Trees.SingletonTypeTree[T]
    type SelectFromTypeTree = Trees.SelectFromTypeTree[T]
    type AndTypeTree = Trees.AndTypeTree[T]
    type OrTypeTree = Trees.OrTypeTree[T]
    type RefineTypeTree = Trees.RefineTypeTree[T]
    type AppliedTypeTree = Trees.AppliedTypeTree[T]
    type TypeBoundsTree = Trees.TypeBoundsTree[T]
    type Bind = Trees.Bind[T]
    type Alternative = Trees.Alternative[T]
    type UnApply = Trees.UnApply[T]
    type ValDef = Trees.ValDef[T]
    type DefDef = Trees.DefDef[T]
    type TypeDef = Trees.TypeDef[T]
    type Template = Trees.Template[T]
    type ClassDef = Trees.ClassDef[T]
    type Import = Trees.Import[T]
    type PackageDef = Trees.PackageDef[T]
    type Annotated = Trees.Annotated[T]
    type EmptyTree = Trees.EmptyTree[T]
    type SharedTree = Trees.SharedTree[T]

    protected implicit def pos(implicit ctx: Context): Position = ctx.position

    def defPos(sym: Symbol)(implicit ctx: Context) = ctx.position union sym.coord.toPosition
  }

  // ----- Helper functions and classes ---------------------------------------

  @tailrec final def unionPos(base: Position, trees: List[Tree[_]]): Position = trees match {
    case t :: ts => unionPos(base union t.xpos, ts)
    case nil => base
  }

  implicit class TreeCopier[T >: Untyped](val tree: Tree[T]) extends AnyVal {
    implicit def cpos = tree.pos
    def derivedIdent(name: Name): Ident[T] = tree match {
      case tree: BackquotedIdent[_] =>
        if (name == tree.name) tree
        else new BackquotedIdent[T](name).copyAttr(tree)
      case tree: Ident[_] if (name == tree.name) => tree
      case _ => Ident[T](name).copyAttr(tree)
    }
    def derivedSelect(qualifier: Tree[T], name: Name): Select[T] = tree match {
      case tree: Select[_] if (qualifier eq tree.qualifier) && (name == tree.name) => tree
      case _ => Select(qualifier, name).copyAttr(tree)
    }
    def derivedThis(qual: TypeName): This[T] = tree match {
      case tree: This[_] if (qual == tree.qual) => tree
      case _ => This[T](qual).copyAttr(tree)
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
      case _ => Literal[T](const).copyAttr(tree)
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
    def derivedReturn(expr: Tree[T], from: Tree[T]): Return[T] = tree match {
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
    def derivedRefineTypeTree(tpt: Tree[T], refinements: List[Tree[T]]): RefineTypeTree[T] = tree match {
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
    def derivedTypeDef(mods: Modifiers[T], name: TypeName, tparams: List[TypeDef[T]], rhs: Tree[T]): TypeDef[T] = tree match {
      case tree: TypeDef[_] if (mods == tree.mods) && (name == tree.name) && (tparams eq tree.tparams) && (rhs eq tree.rhs) => tree
      case _ => TypeDef(mods, name, tparams, rhs).copyAttr(tree)
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
    def derivedSharedTree(shared: Tree[T]): SharedTree[T] = tree match {
      case tree: SharedTree[_] if (shared eq tree.shared) => tree
      case _ => SharedTree(shared).copyAttr(tree)
    }
  }

  abstract class FullTreeTransformer[T >: Untyped, C] {
    var sharedMemo: Map[SharedTree[T], SharedTree[T]] = Map()

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
        finishReturn(tree.derivedReturn(transform(expr, c), transform(from, c)), tree, c, plugins)
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
      case TypeDef(mods, name, tparams, rhs) =>
        finishTypeDef(tree.derivedTypeDef(mods, name, transformSub(tparams, c), transform(rhs, c)), tree, c, plugins)
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
      case tree @ SharedTree(shared) =>
        finishSharedTree(
          sharedMemo get tree match {
          case Some(tree1) => tree1
          case None =>
            val tree1 = tree.derivedSharedTree(transform(shared, c))
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
    def finishSharedTree(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
  }

  abstract class TreeTransformer[T >: Untyped] {
    var sharedMemo: Map[SharedTree[T], SharedTree[T]] = Map()

    def transform(tree: Tree[T]): Tree[T] = tree match {
      case Ident(name) =>
        tree
      case Select(qualifier, name) =>
        tree.derivedSelect(transform(qualifier), name)
      case This(qual) =>
        tree
      case Super(qual, mix) =>
        tree.derivedSuper(transform(qual), mix)
      case Apply(fun, args) =>
        tree.derivedApply(transform(fun), transform(args))
      case TypeApply(fun, args) =>
        tree.derivedTypeApply(transform(fun), transform(args))
      case Literal(const) =>
        tree
      case New(tpt) =>
        tree.derivedNew(transform(tpt))
      case Pair(left, right) =>
        tree.derivedPair(transform(left), transform(right))
      case Typed(expr, tpt) =>
        tree.derivedTyped(transform(expr), transform(tpt))
      case NamedArg(name, arg) =>
        tree.derivedNamedArg(name, transform(arg))
      case Assign(lhs, rhs) =>
        tree.derivedAssign(transform(lhs), transform(rhs))
      case Block(stats, expr) =>
        tree.derivedBlock(transform(stats), transform(expr))
      case If(cond, thenp, elsep) =>
        tree.derivedIf(transform(cond), transform(thenp), transform(elsep))
      case Match(selector, cases) =>
        tree.derivedMatch(transform(selector), transformSub(cases))
      case CaseDef(pat, guard, body) =>
        tree.derivedCaseDef(transform(pat), transform(guard), transform(body))
      case Return(expr, from) =>
        tree.derivedReturn(transform(expr), transformSub(from))
      case Try(block, catches, finalizer) =>
        tree.derivedTry(transform(block), transformSub(catches), transform(finalizer))
      case Throw(expr) =>
        tree.derivedThrow(transform(expr))
      case SeqLiteral(elemtpt, elems) =>
        tree.derivedSeqLiteral(transform(elemtpt), transform(elems))
      case TypeTree(original) =>
        tree.derivedTypeTree(transform(original))
      case SingletonTypeTree(ref) =>
        tree.derivedSingletonTypeTree(transform(ref))
      case SelectFromTypeTree(qualifier, name) =>
        tree.derivedSelectFromTypeTree(transform(qualifier), name)
      case AndTypeTree(left, right) =>
        tree.derivedAndTypeTree(transform(left), transform(right))
      case OrTypeTree(left, right) =>
        tree.derivedOrTypeTree(transform(left), transform(right))
      case RefineTypeTree(tpt, refinements) =>
        tree.derivedRefineTypeTree(transform(tpt), transformSub(refinements))
      case AppliedTypeTree(tpt, args) =>
        tree.derivedAppliedTypeTree(transform(tpt), transform(args))
      case TypeBoundsTree(lo, hi) =>
        tree.derivedTypeBoundsTree(transform(lo), transform(hi))
      case Bind(name, body) =>
        tree.derivedBind(name, transform(body))
      case Alternative(trees) =>
        tree.derivedAlternative(transform(trees))
      case UnApply(fun, args) =>
        tree.derivedUnApply(transform(fun), transform(args))
      case ValDef(mods, name, tpt, rhs) =>
        tree.derivedValDef(mods, name, transform(tpt), transform(rhs))
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        tree.derivedDefDef(mods, name, transformSub(tparams), vparamss mapConserve (transformSub(_)), transform(tpt), transform(rhs))
      case TypeDef(mods, name, tparams, rhs) =>
        tree.derivedTypeDef(mods, name, transformSub(tparams), transform(rhs))
      case Template(parents, self, body) =>
        tree.derivedTemplate(transform(parents), transformSub(self), transform(body))
      case ClassDef(mods, name, tparams, impl) =>
        tree.derivedClassDef(mods, name, transformSub(tparams), transformSub(impl))
      case Import(expr, selectors) =>
        tree.derivedImport(transform(expr), selectors)
      case PackageDef(pid, stats) =>
        tree.derivedPackageDef(transformSub(pid), transform(stats))
      case Annotated(annot, arg) =>
        tree.derivedAnnotated(transform(annot), transform(arg))
      case EmptyTree() =>
        tree
      case tree @ SharedTree(shared) =>
        sharedMemo get tree match {
          case Some(tree1) => tree1
          case None =>
            val tree1 = tree.derivedSharedTree(transform(shared))
            sharedMemo = sharedMemo.updated(tree, tree1)
            tree1
        }
    }
    def transform(trees: List[Tree[T]]): List[Tree[T]] =
      trees mapConserve (transform(_))
    def transformSub(tree: Tree[T]): tree.ThisTree[T] =
      transform(tree).asInstanceOf[tree.ThisTree[T]]
    def transformSub[TT <: Tree[T]](trees: List[TT]): List[TT] =
      transform(trees).asInstanceOf[List[TT]]
  }

  abstract class TreeAccumulator[X, T >: Untyped] extends ((X, Tree[T]) => X) {
    var sharedMemo: Map[SharedTree[T], X] = Map()
    def apply(x: X, tree: Tree[T]): X
    def apply(x: X, trees: List[Tree[T]]): X = (x /: trees)(apply)
    def foldOver(x: X, tree: Tree[T]): X = tree match {
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
      case TypeDef(mods, name, tparams, rhs) =>
        this(this(x, tparams), rhs)
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
      case tree @ SharedTree(shared) =>
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
  class DeepFolder[X, T >: Untyped](f: (X, Tree[T]) => X) extends TreeAccumulator[X, T] {
    def apply(x: X, tree: Tree[T]): X = foldOver(f(x, tree), tree)
  }

  /** Fold `f` over all tree nodes, in depth-first, prefix order, but don't visit
   *  subtrees where `f` returns a different result for the root, i.e. `f(x, root) ne x`.
   */
  class ShallowFolder[X, T >: Untyped](f: (X, Tree[T]) => X) extends TreeAccumulator[X, T] {
    def apply(x: X, tree: Tree[T]): X = {
      val x1 = f(x, tree)
      if (x1.asInstanceOf[AnyRef] ne x1.asInstanceOf[AnyRef]) x1
      else foldOver(x1, tree)
    }
  }
}
