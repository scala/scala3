package dotty.tools.dotc
package ast

import core._
import Types._, Names._, Flags._, util.Positions._, Contexts._, Constants._, SymDenotations._, Symbols._
import Denotations._, StdNames._
import annotation.tailrec
import language.higherKinds
import collection.IndexedSeqOptimized
import collection.immutable.IndexedSeq
import collection.mutable.ListBuffer
import parsing.Tokens.Token
import printing.Printer
import util.{Stats, Attachment, DotClass}
import annotation.unchecked.uncheckedVariance

object Trees {

  // Note: it would be more logical to make Untyped = Nothing.
  // However, this interacts in a bad way with Scala's current type inference.
  // In fact, we cannot write soemthing like Select(pre, name), where pre is
  // of type Tree[Nothing]; type inference will treat the Nothing as an uninstantited
  // value and will not infer Nothing as the type parameter for Select.
  // We should come back to this issue once type inference is changed.
  type Untyped = Null

  /** The total number of created tree nodes, maintained if Stats.enabled */
  var ntrees = 0

  /** A base class for things that have positions (currently: modifiers and trees)
   */
  abstract class Positioned extends DotClass with Product {

    private[this] var curPos: Position = _

    setPos(initialPos)

    /** The item's position.
     */
    def pos: Position = curPos

    /** Destructively update `curPos` to given position. Also, set any missing
     *  positions in children.
     */
    protected def setPos(pos: Position): Unit = {
      curPos = pos
      if (pos.exists) setChildPositions(pos.toSynthetic)
    }

    /** The envelope containing the item in its entirety. Envelope is different from
     *  `pos` for definitions (instances of MemberDef).
     */
    def envelope: Position = pos.toSynthetic

    /** A positioned item like this one with the position set to `pos`.
     *  if the positioned item is source-derived, a clone is returned.
     *  If the positioned item is synthetic, the position is updated
     *  destructively and the item itself is returned.
     */
    def withPos(pos: Position): this.type = {
      val newpd = (if (pos == curPos || curPos.isSynthetic) this else clone).asInstanceOf[Positioned]
      newpd.setPos(pos)
      newpd.asInstanceOf[this.type]
    }

    def withPos(posd: Positioned): this.type =
      if (posd == null) this else withPos(posd.pos)

    /** This item with a position that's the union of the given `pos` and the
     *  current position.
     */
    def addPos(pos: Position): this.type = withPos(pos union this.pos)

    /** If any children of this node do not have positions, set them to the given position,
     *  and transitively visit their children.
     */
    private def setChildPositions(pos: Position): Unit = {
      def deepSetPos(x: Any): Unit = x match {
        case p: Positioned =>
          if (!p.pos.exists) p.setPos(pos)
        case xs: List[_] =>
          xs foreach deepSetPos
        case _ =>
      }
      var n = productArity
      while (n > 0) {
        n -= 1
        deepSetPos(productElement(n))
      }
    }

    /** The initial, synthetic position. This is usually the union of all positioned children's
     *  envelopes.
     */
    protected def initialPos: Position = {
      var n = productArity
      var pos = NoPosition
      while (n > 0) {
        n -= 1
        productElement(n) match {
          case p: Positioned => pos = pos union p.envelope
          case xs: List[_] => pos = unionPos(pos, xs)
          case _ =>
        }
      }
      pos.toSynthetic
    }

    private def unionPos(pos: Position, xs: List[_]): Position = xs match {
      case (t: Tree[_]) :: xs1 => unionPos(pos union t.envelope, xs1)
      case _ => pos
    }

    def contains(that: Positioned): Boolean = {
      def isParent(x: Any): Boolean = x match {
        case x: Positioned =>
          x contains that
        case xs: List[_] =>
          xs exists isParent
        case _ =>
          false
      }
      (this eq that) ||
      (this.envelope contains that.pos) && {
        var n = productArity
        var found = false
        while (n > 0 && !found) {
          n -= 1
          found = isParent(productElement(n))
        }
        found
      }
    }
  }

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
   *                        If there is no opening keyword, point should equal end.
   */
  case class Modifiers[-T >: Untyped] (
    flags: FlagSet = EmptyFlags,
    privateWithin: TypeName = tpnme.EMPTY,
    annotations: List[Tree[T]] = Nil) extends Positioned with Cloneable {

    def is(fs: FlagSet): Boolean = flags is fs
    def is(fc: FlagConjunction): Boolean = flags is fc

    def | (fs: FlagSet): Modifiers[T] = withFlags(flags | fs)
    def & (fs: FlagSet): Modifiers[T] = withFlags(flags & fs)
    def &~(fs: FlagSet): Modifiers[T] = withFlags(flags &~ fs)

    def toTypeFlags: Modifiers[T] = withFlags(flags.toTypeFlags)
    def toTermFlags: Modifiers[T] = withFlags(flags.toTermFlags)

    private def withFlags(flags: FlagSet) =
      if (this.flags == flags) this
      else copy(flags = flags)

    def withAnnotations[U >: Untyped <: T](annots: List[Tree[U]]): Modifiers[U] =
      if (annots.isEmpty) this
      else copy(annotations = annotations ++ annots)

    def withPrivateWithin(pw: TypeName) =
      if (pw.isEmpty) this
      else copy(privateWithin = pw)

    def hasFlags = flags != EmptyFlags
    def hasAnnotations = annotations.nonEmpty
    def hasPrivateWithin = privateWithin != tpnme.EMPTY

    def tokenPos: Seq[(Token, Position)] = ???
  }

  private var nextId = 0

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
  abstract class Tree[-T >: Untyped] extends Positioned
                                        with Product
                                        with Attachment.Container
                                        with printing.Showable
                                        with Cloneable {

    if (Stats.enabled) ntrees += 1

    /** A unique identifier for this tree. Used for debugging, and potentially
     *  tracking presentation compiler interactions
     */
    val uniqueId = {
      nextId += 1
      //assert(nextId != 214, this)
      nextId
    }

    /** The type  constructor at the root of the tree */
    type ThisTree[T >: Untyped] <: Tree[T]

    private[this] var myTpe: T = _

    /** Destructively set the type of the tree. This should be called only when it is known that
     *  it is safe under sharing to do so. One user-case is in the withType method below
     *  which implements copy-on-write. Another use-case is in method interpolateAndAdapt in Typer,
     *  where we overwrite with a simplified version of the type itself.
     */
    private[dotc] def overwriteType(tpe: T) = myTpe = tpe

    /** The type of the tree. In case of an untyped tree,
     *   an UnAssignedTypeException is thrown. (Overridden by empty trees)
     */
    def tpe: T @uncheckedVariance = {
      if (myTpe == null) throw new UnAssignedTypeException(this)
      myTpe
    }

    /** Copy `tpe` attribute from tree `from` into this tree, independently
     *  whether it is null or not.
    final def copyAttr[U >: Untyped](from: Tree[U]): ThisTree[T] = {
      val t1 = this.withPos(from.pos)
      val t2 =
        if (from.myTpe != null) t1.withType(from.myTpe.asInstanceOf[Type])
        else t1
      t2.asInstanceOf[ThisTree[T]]
    }
     */

    /** Return a typed tree that's isomorphic to this tree, but has given
     *  type. (Overridden by empty trees)
     */
    def withType(tpe: Type)(implicit ctx: Context): ThisTree[Type] = {
      if (tpe == ErrorType) assert(ctx.errorsReported)
      withTypeUnchecked(tpe)
    }

    def withTypeUnchecked(tpe: Type): ThisTree[Type] = {
      val tree =
        (if (myTpe == null ||
          (myTpe.asInstanceOf[AnyRef] eq tpe.asInstanceOf[AnyRef])) this
         else clone).asInstanceOf[Tree[Type]]
      tree overwriteType tpe
      tree.asInstanceOf[ThisTree[Type]]
    }

    /** Does the tree have its type field set? Note: this operation is not
     *  referentially transparent, because it can observe the withType
     *  modifications. Should be used only in special circumstances (we
     *  need it for printing trees with optional type info).
     */
    final def hasType: Boolean = myTpe != null

    final def typeOpt: Type = myTpe match {
      case tp: Type => tp
      case _ => NoType
    }

    /** The denotation referred tno by this tree.
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

    /** Convert tree to a list. Gives a singleton list, except
     *  for thickets which return their element trees.
     */
    def toList: List[Tree[T]] = this :: Nil

    /** if this tree is the empty tree, the alternative, else this tree */
    def orElse[U >: Untyped <: T](that: => Tree[U]): Tree[U] =
      if (this eq genericEmptyTree) that else this

    /** The number of nodes in this tree */
    def treeSize: Int = {
      var s = 1
      def addSize(elem: Any): Unit = elem match {
        case t: Tree[_] => s += t.treeSize
        case ts: List[_] => ts foreach addSize
        case _ =>
      }
      productIterator foreach addSize
      s
    }

    override def toText(printer: Printer) = printer.toText(this)

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
  trait TypTree[-T >: Untyped] extends Tree[T] {
    type ThisTree[-T >: Untyped] <: TypTree[T]
    override def isType = true
  }

  /** Instances of this class are trees for which isTerm is definitely true.
   *  Note that some trees have isTerm = true without being TermTrees (e.g. Ident, AnnotatedTree)
   */
  trait TermTree[-T >: Untyped] extends Tree[T] {
    type ThisTree[-T >: Untyped] <: TermTree[T]
    override def isTerm = true
  }

  /** Instances of this class are trees which are not terms but are legal
   *  parts of patterns.
   */
  trait PatternTree[-T >: Untyped] extends Tree[T] {
    type ThisTree[-T >: Untyped] <: PatternTree[T]
    override def isPattern = true
  }

  /** Tree's denotation can be derived from its type */
  abstract class DenotingTree[-T >: Untyped] extends Tree[T] {
    type ThisTree[-T >: Untyped] <: DenotingTree[T]
    override def denot(implicit ctx: Context) = tpe match {
      case tpe: NamedType => tpe.denot
      case ThisType(cls) => cls.denot
      case _ => NoDenotation
    }
  }

  /** Tree's denot/isType/isTerm properties come from a subtree
   *  identified by `forwardTo`.
   */
  abstract class ProxyTree[-T >: Untyped] extends Tree[T] {
    type ThisTree[-T >: Untyped] <: ProxyTree[T]
    def forwardTo: Tree[T]
    override def denot(implicit ctx: Context): Denotation = forwardTo.denot
    override def isTerm = forwardTo.isTerm
    override def isType = forwardTo.isType
  }

  /** Tree has a name */
  abstract class NameTree[-T >: Untyped] extends DenotingTree[T] {
    type ThisTree[-T >: Untyped] <: NameTree[T]
    def name: Name
    def withName(name1: Name)(implicit ctx: Context): untpd.NameTree
  }

  /** Tree refers by name to a denotation */
  abstract class RefTree[-T >: Untyped] extends NameTree[T] {
    type ThisTree[-T >: Untyped] <: RefTree[T]
    def qualifier: Tree[T]
    override def isType = name.isTypeName
    override def isTerm = name.isTermName
  }

  /** Tree defines a new symbol */
  trait DefTree[-T >: Untyped] extends DenotingTree[T] {
    type ThisTree[-T >: Untyped] <: DefTree[T]
    override def isDef = true
    def namedType = tpe.asInstanceOf[NamedType]
  }

  /** Tree defines a new symbol and carries modifiers.
   *  The position of a MemberDef contains only the defined identifier or pattern.
   *  The envelope of a MemberDef contains the whole definition and his its point
   *  on the opening keyword (or the next token after that if keyword is missing).
   */
  trait MemberDef[-T >: Untyped] extends NameTree[T] with DefTree[T] {
    type ThisTree[-T >: Untyped] <: MemberDef[T]
    def mods: Modifiers[T]
    override def envelope: Position = mods.pos union pos union initialPos
  }

  /** A ValDef or DefDef tree */
  trait ValOrDefDef[-T >: Untyped] extends MemberDef[T] {
    def tpt: Tree[T]
    def rhs: Tree[T]
  }

  // ----------- Tree case classes ------------------------------------

  /** name */
  case class Ident[-T >: Untyped] private[ast] (name: Name)
    extends RefTree[T] {
    type ThisTree[-T >: Untyped] = Ident[T]
    def withName(name: Name)(implicit ctx: Context): untpd.Ident = untpd.cpy.Ident(this, name)
    def qualifier: Tree[T] = genericEmptyTree
  }

  class BackquotedIdent[-T >: Untyped] private[ast] (name: Name)
    extends Ident[T](name)

  /** qualifier.name */
  case class Select[-T >: Untyped] private[ast] (qualifier: Tree[T], name: Name)
    extends RefTree[T] {
    type ThisTree[-T >: Untyped] = Select[T]
    def withName(name: Name)(implicit ctx: Context): untpd.Select = untpd.cpy.Select(this, qualifier, name)
  }

  class SelectWithSig[-T >: Untyped] private[ast] (qualifier: Tree[T], name: Name, val sig: Signature)
    extends Select[T](qualifier, name)

  /** qual.this */
  case class This[-T >: Untyped] private[ast] (qual: TypeName)
    extends DenotingTree[T] with TermTree[T] {
    type ThisTree[-T >: Untyped] = This[T]
  }

  /** C.super[mix], where qual = C.this */
  case class Super[-T >: Untyped] private[ast] (qual: Tree[T], mix: TypeName)
    extends ProxyTree[T] with TermTree[T] {
    type ThisTree[-T >: Untyped] = Super[T]
    def forwardTo = qual
  }

  abstract class GenericApply[-T >: Untyped] extends ProxyTree[T] with TermTree[T] {
    type ThisTree[-T >: Untyped] <: GenericApply[T]
    val fun: Tree[T]
    val args: List[Tree[T]]
    def forwardTo = fun
  }

  /** fun(args) */
  case class Apply[-T >: Untyped] private[ast] (fun: Tree[T], args: List[Tree[T]])
    extends GenericApply[T] {
    type ThisTree[-T >: Untyped] = Apply[T]
  }

  /** fun[args] */
  case class TypeApply[-T >: Untyped] private[ast] (fun: Tree[T], args: List[Tree[T]])
    extends GenericApply[T] {
    type ThisTree[-T >: Untyped] = TypeApply[T]
  }

  /** const */
  case class Literal[-T >: Untyped] private[ast] (const: Constant)
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Literal[T]
  }

  /** new tpt, but no constructor call */
  case class New[-T >: Untyped] private[ast] (tpt: Tree[T])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = New[T]
  }

  /** (left, right) */
  case class Pair[-T >: Untyped] private[ast] (left: Tree[T], right: Tree[T])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Pair[T]
    override def isTerm = left.isTerm && right.isTerm
    override def isType = left.isType && right.isType
    override def isPattern = !isTerm && (left.isPattern || left.isTerm) && (right.isPattern || right.isTerm)
  }

  /** expr : tpt */
  case class Typed[-T >: Untyped] private[ast] (expr: Tree[T], tpt: Tree[T])
    extends ProxyTree[T] with TermTree[T] {
    type ThisTree[-T >: Untyped] = Typed[T]
    def forwardTo = expr
  }

  /** name = arg, in a parameter list */
  case class NamedArg[-T >: Untyped] private[ast] (name: Name, arg: Tree[T])
    extends Tree[T] {
    type ThisTree[-T >: Untyped] = NamedArg[T]
  }

  /** name = arg, outside a parameter list */
  case class Assign[-T >: Untyped] private[ast] (lhs: Tree[T], rhs: Tree[T])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Assign[T]
  }

  /** { stats; expr } */
  case class Block[-T >: Untyped] private[ast] (stats: List[Tree[T]], expr: Tree[T])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Block[T]
  }

  /** if cond then thenp else elsep */
  case class If[-T >: Untyped] private[ast] (cond: Tree[T], thenp: Tree[T], elsep: Tree[T])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = If[T]
  }

  /** A closure with an environment and a reference to a method.
   *  @param env    The captured parameters of the closure
   *  @param meth   A ref tree that refers to the method of the closure.
   *                The first (env.length) parameters of that method are filled
   *                with env values.
   *  @param tpt    Either EmptyTree or a TypeTree. If tpt is EmptyTree the type
   *                of the closure is a function type, otherwise it is the type
   *                given in `tpt`, which must be a SAM type.
   */
  case class Closure[-T >: Untyped] private[ast] (env: List[Tree[T]], meth: Tree[T], tpt: Tree[T])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Closure[T]
  }

  /** selector match { cases } */
  case class Match[-T >: Untyped] private[ast] (selector: Tree[T], cases: List[CaseDef[T]])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Match[T]
  }

  /** case pat if guard => body; only appears as child of a Match */
  case class CaseDef[-T >: Untyped] private[ast] (pat: Tree[T], guard: Tree[T], body: Tree[T])
    extends Tree[T] {
    type ThisTree[-T >: Untyped] = CaseDef[T]
  }

  /** return expr
   *  where `from` refers to the method from which the return takes place
   *  After program transformations this is not necessarily the enclosing method, because
   *  closures can intervene.
   */
  case class Return[-T >: Untyped] private[ast] (expr: Tree[T], from: Tree[T] = genericEmptyTree)
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Return[T]
  }

  /** try block catch handler finally finalizer
   *
   *  Note: if the handler is a case block CASES of the form
   *
   *    { case1 ... caseN }
   *
   *  the parser returns Match(EmptyTree, CASES). Desugaring and typing this yields a closure
   *  node
   *
   *    { def $anonfun(x: Throwable) = x match CASES; Closure(Nil, $anonfun) }
   *
   *  At some later stage when we normalize the try we can revert this to
   *
   *    Match(EmptyTree, CASES)
   *
   *  or else if stack is non-empty
   *
   *    Match(EmptyTree, <case x: Throwable => $anonfun(x)>)
   */
  case class Try[-T >: Untyped] private[ast] (expr: Tree[T], handler: Tree[T], finalizer: Tree[T])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Try[T]
  }

  /** throw expr */
  case class Throw[-T >: Untyped] private[ast] (expr: Tree[T])
    extends TermTree[T] {
    type ThisTree[-T >: Untyped] = Throw[T]
  }

  /** Seq(elems) */
  case class SeqLiteral[-T >: Untyped] private[ast] (elems: List[Tree[T]])
    extends Tree[T] {
    type ThisTree[-T >: Untyped] = SeqLiteral[T]
  }

  /** Array(elems) */
  class JavaSeqLiteral[T >: Untyped] private[ast] (elems: List[Tree[T]])
    extends SeqLiteral(elems) {
  }

  /** A type tree that represents an existing or inferred type */
  case class TypeTree[-T >: Untyped] private[ast] (original: Tree[T])
    extends DenotingTree[T] with TypTree[T] {
    type ThisTree[-T >: Untyped] = TypeTree[T]
    override def initialPos = NoPosition
    override def isEmpty = !hasType && original.isEmpty
    override def toString =
      s"TypeTree${if (hasType) s"[$typeOpt]" else s"($original)"}"
  }

  /** ref.type */
  case class SingletonTypeTree[-T >: Untyped] private[ast] (ref: Tree[T])
    extends DenotingTree[T] with TypTree[T] {
    type ThisTree[-T >: Untyped] = SingletonTypeTree[T]
  }

  /** qualifier # name */
  case class SelectFromTypeTree[-T >: Untyped] private[ast] (qualifier: Tree[T], name: Name)
    extends RefTree[T] {
    type ThisTree[-T >: Untyped] = SelectFromTypeTree[T]
    def withName(name: Name)(implicit ctx: Context): untpd.SelectFromTypeTree = untpd.cpy.SelectFromTypeTree(this, qualifier, name)
  }

  /** left & right */
  case class AndTypeTree[-T >: Untyped] private[ast] (left: Tree[T], right: Tree[T])
    extends TypTree[T] {
    type ThisTree[-T >: Untyped] = AndTypeTree[T]
  }

  /** left | right */
  case class OrTypeTree[-T >: Untyped] private[ast] (left: Tree[T], right: Tree[T])
    extends TypTree[T] {
    type ThisTree[-T >: Untyped] = OrTypeTree[T]
  }

  /** tpt { refinements } */
  case class RefinedTypeTree[-T >: Untyped] private[ast] (tpt: Tree[T], refinements: List[Tree[T]])
    extends ProxyTree[T] with TypTree[T] {
    type ThisTree[-T >: Untyped] = RefinedTypeTree[T]
    def forwardTo = tpt
  }

  /** tpt[args] */
  case class AppliedTypeTree[-T >: Untyped] private[ast] (tpt: Tree[T], args: List[Tree[T]])
    extends ProxyTree[T] with TypTree[T] {
    type ThisTree[-T >: Untyped] = AppliedTypeTree[T]
    def forwardTo = tpt
  }

  /** => T */
  case class ByNameTypeTree[-T >: Untyped] private[ast] (result: Tree[T])
  extends TypTree[T] {
    type ThisTree[-T >: Untyped] = ByNameTypeTree[T]
  }

  /** >: lo <: hi */
  case class TypeBoundsTree[-T >: Untyped] private[ast] (lo: Tree[T], hi: Tree[T])
    extends TypTree[T] {
    type ThisTree[-T >: Untyped] = TypeBoundsTree[T]
  }

  /** name @ body */
  case class Bind[-T >: Untyped] private[ast] (name: Name, body: Tree[T])
    extends NameTree[T] with DefTree[T] with PatternTree[T] {
    type ThisTree[-T >: Untyped] = Bind[T]
    override def envelope: Position = pos union initialPos
    def withName(name: Name)(implicit ctx: Context): untpd.Bind = untpd.cpy.Bind(this, name, body)
  }

  /** tree_1 | ... | tree_n */
  case class Alternative[-T >: Untyped] private[ast] (trees: List[Tree[T]])
    extends PatternTree[T] {
    type ThisTree[-T >: Untyped] = Alternative[T]
  }

  /** The typed translation of `extractor(patterns)` in a pattern. The translation has the following
   *  components:
   *
   *  @param fun       is `extractor.unapply` (or, for backwards compatibility, `extractor.unapplySeq`)
   *                   possibly with type parameters
   *  @param implicits Any implicit parameters passed to the unapply after the selector
   *  @param patterns  The argument patterns in the pattern match.
   *
   *  Given a match selector `sel` a pattern UnApply(fun, implicits, patterns) is roughly translated as follows
   *
   *    val result = fun(sel)(implicits)
   *    if (result.isDefined) "match patterns against result"
   */
  case class UnApply[-T >: Untyped] private[ast] (fun: Tree[T], implicits: List[Tree[T]], patterns: List[Tree[T]])
    extends PatternTree[T] {
    type ThisTree[-T >: Untyped] = UnApply[T]
  }

  /** mods val name: tpt = rhs */
  case class ValDef[-T >: Untyped] private[ast] (mods: Modifiers[T], name: TermName, tpt: Tree[T], rhs: Tree[T])
    extends ValOrDefDef[T] {
    type ThisTree[-T >: Untyped] = ValDef[T]
    def withName(name: Name)(implicit ctx: Context): untpd.ValDef = untpd.cpy.ValDef(this, mods, name.toTermName, tpt, rhs)
    assert(isEmpty || tpt != genericEmptyTree)
  }

  /** mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs */
  case class DefDef[-T >: Untyped] private[ast] (mods: Modifiers[T], name: TermName, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T])
    extends ValOrDefDef[T] {
    type ThisTree[-T >: Untyped] = DefDef[T]
    def withName(name: Name)(implicit ctx: Context): untpd.DefDef = untpd.cpy.DefDef(this, mods, name.toTermName, tparams, vparamss, tpt, rhs)
    assert(tpt != genericEmptyTree)
  }

  /** mods class name template     or
   *  mods trait name template     or
   *  mods type name = rhs   or
   *  mods type name >: lo <: hi, if rhs = TypeBoundsTree(lo, hi) & (lo ne hi)
   */
  case class TypeDef[-T >: Untyped] private[ast] (mods: Modifiers[T], name: TypeName, rhs: Tree[T])
    extends MemberDef[T] {
    type ThisTree[-T >: Untyped] = TypeDef[T]
    def withName(name: Name)(implicit ctx: Context): untpd.TypeDef = untpd.cpy.TypeDef(this, mods, name.toTypeName, rhs, tparams)

    /** Is this a definition of a class? */
    def isClassDef = rhs.isInstanceOf[Template[_]]

    /** If this a non-class type definition, its type parameters.
     *  Can be different from Nil only for PolyTypeDefs, which are always
     *  untyped and get eliminated during desugaring.
     */
    def tparams: List[untpd.TypeDef] = Nil
  }

  /** extends parents { self => body } */
  case class Template[-T >: Untyped] private[ast] (constr: DefDef[T], parents: List[Tree[T]], self: ValDef[T], body: List[Tree[T]])
    extends DefTree[T] {
    type ThisTree[-T >: Untyped] = Template[T]
  }

  /** import expr.selectors
   *  where a selector is either an untyped `Ident`, `name` or
   *  an untyped `Pair` `name => rename`
   */
  case class Import[-T >: Untyped] private[ast] (expr: Tree[T], selectors: List[Tree[Untyped]])
    extends DenotingTree[T] {
    type ThisTree[-T >: Untyped] = Import[T]
  }

  /** package pid { stats } */
  case class PackageDef[-T >: Untyped] private[ast] (pid: RefTree[T], stats: List[Tree[T]])
    extends ProxyTree[T] {
    type ThisTree[-T >: Untyped] = PackageDef[T]
    def forwardTo = pid
  }

  /** arg @annot */
  case class Annotated[-T >: Untyped] private[ast] (annot: Tree[T], arg: Tree[T])
    extends ProxyTree[T] {
    type ThisTree[-T >: Untyped] = Annotated[T]
    def forwardTo = arg
  }

  trait WithoutTypeOrPos[-T >: Untyped] extends Tree[T] {
    override def tpe: T @uncheckedVariance = NoType.asInstanceOf[T]
    override def withTypeUnchecked(tpe: Type) = this.asInstanceOf[ThisTree[Type]]
    override def pos = NoPosition
    override def setPos(pos: Position) = {}
  }

  /** Temporary class that results from translation of ModuleDefs
   *  (and possibly other statements).
   *  The contained trees will be integrated when transformed with
   *  a `transform(List[Tree])` call.
   */
  case class Thicket[-T >: Untyped](trees: List[Tree[T]])
    extends Tree[T] with WithoutTypeOrPos[T] {
    type ThisTree[-T >: Untyped] = Thicket[T]
    override def isEmpty: Boolean = trees.isEmpty
    override def toList: List[Tree[T]] = flatten(trees)
    override def toString = if (isEmpty) "EmptyTree" else "Thicket(" + trees.mkString(", ") + ")"
    override def withPos(pos: Position): this.type = {
      val newTrees = trees.map(_.withPos(pos))
      new Thicket[T](newTrees).asInstanceOf[this.type]
    }
  }

  class EmptyValDef[T >: Untyped] extends ValDef[T](
    Modifiers[T](Private), nme.WILDCARD, genericEmptyTree[T], genericEmptyTree[T]) with WithoutTypeOrPos[T] {
    override def isEmpty: Boolean = true
  }

  val theEmptyTree: Thicket[Type] = Thicket(Nil)
  val theEmptyValDef = new EmptyValDef[Type]

  def genericEmptyValDef[T >: Untyped]: ValDef[T] = theEmptyValDef.asInstanceOf[ValDef[T]]
  def genericEmptyTree[T >: Untyped]: Thicket[T] = theEmptyTree.asInstanceOf[Thicket[T]]

  def flatten[T >: Untyped](trees: List[Tree[T]]): List[Tree[T]] = {
    var buf: ListBuffer[Tree[T]] = null
    var xs = trees
    while (xs.nonEmpty) {
      xs.head match {
        case Thicket(elems) =>
          if (buf == null) {
            buf = new ListBuffer
            var ys = trees
            while (ys ne xs) {
              buf += ys.head
              ys = ys.tail
            }
          }
          for (elem <- elems) {
            assert(!elem.isInstanceOf[Thicket[_]])
            buf += elem
          }
        case tree =>
          if (buf != null) buf += tree
      }
      xs = xs.tail
    }
    if (buf != null) buf.toList else trees
  }

  // ----- Generic Tree Instances, inherited from  `tpt` and `untpd`.

  abstract class Instance[T >: Untyped <: Type] extends DotClass { inst =>

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
    type MemberDef = Trees.MemberDef[T]
    type ValOrDefDef = Trees.ValOrDefDef[T]

    type Ident = Trees.Ident[T]
    type BackquotedIdent = Trees.BackquotedIdent[T]
    type Select = Trees.Select[T]
    type SelectWithSig = Trees.SelectWithSig[T]
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
    type Closure = Trees.Closure[T]
    type Match = Trees.Match[T]
    type CaseDef = Trees.CaseDef[T]
    type Return = Trees.Return[T]
    type Try = Trees.Try[T]
    type Throw = Trees.Throw[T]
    type SeqLiteral = Trees.SeqLiteral[T]
    type JavaSeqLiteral = Trees.JavaSeqLiteral[T]
    type TypeTree = Trees.TypeTree[T]
    type SingletonTypeTree = Trees.SingletonTypeTree[T]
    type SelectFromTypeTree = Trees.SelectFromTypeTree[T]
    type AndTypeTree = Trees.AndTypeTree[T]
    type OrTypeTree = Trees.OrTypeTree[T]
    type RefinedTypeTree = Trees.RefinedTypeTree[T]
    type AppliedTypeTree = Trees.AppliedTypeTree[T]
    type ByNameTypeTree = Trees.ByNameTypeTree[T]
    type TypeBoundsTree = Trees.TypeBoundsTree[T]
    type Bind = Trees.Bind[T]
    type Alternative = Trees.Alternative[T]
    type UnApply = Trees.UnApply[T]
    type ValDef = Trees.ValDef[T]
    type DefDef = Trees.DefDef[T]
    type TypeDef = Trees.TypeDef[T]
    type Template = Trees.Template[T]
    type Import = Trees.Import[T]
    type PackageDef = Trees.PackageDef[T]
    type Annotated = Trees.Annotated[T]
    type Thicket = Trees.Thicket[T]

    val EmptyTree: Thicket = genericEmptyTree
    val EmptyValDef: ValDef = genericEmptyValDef

    // ----- Auxiliary creation methods ------------------

    def Modifiers(flags: FlagSet = EmptyFlags,
                  privateWithin: TypeName = tpnme.EMPTY,
                  annotations: List[Tree] = Nil) = new Modifiers(flags, privateWithin, annotations)

    def Thicket(trees: List[Tree]): Thicket = new Thicket(trees)
    def Thicket(): Thicket = EmptyTree
    def Thicket(x1: Tree, x2: Tree): Thicket = Thicket(x1 :: x2 :: Nil)
    def Thicket(x1: Tree, x2: Tree, x3: Tree): Thicket = Thicket(x1 :: x2 :: x3 :: Nil)
    def flatTree(xs: List[Tree]): Tree = flatten(xs) match {
      case x :: Nil => x
      case ys => Thicket(ys)
    }

    // ----- Position handling -----------------------------------------

    def foreachSubTreeOf(tree: Tree)(f: Tree => Unit): Unit = {
      val traverser = new TreeTraverser {
        def traverse(tree: Tree) = foldOver(f(tree), tree)
      }
      traverser.traverse(tree)
    }

    // ----- Helper classes for copying, transforming, accumulating -----------------

    val cpy: TreeCopier

    abstract class TreeCopier {

      def postProcess(tree: Tree, copied: untpd.Tree): copied.ThisTree[T]

      def finalize(tree: Tree, copied: untpd.Tree): copied.ThisTree[T] =
        postProcess(tree, copied withPos tree.pos)

      def Ident(tree: Tree, name: Name): Ident = tree match {
        case tree: BackquotedIdent =>
          if (name == tree.name) tree
          else finalize(tree, new BackquotedIdent(name))
        case tree: Ident if (name == tree.name) => tree
        case _ => finalize(tree, untpd.Ident(name))
      }
      def Select(tree: Tree, qualifier: Tree, name: Name): Select = tree match {
        case tree: SelectWithSig =>
          if ((qualifier eq tree.qualifier) && (name == tree.name)) tree
          else finalize(tree, new SelectWithSig(qualifier, name, tree.sig))
        case tree: Select if (qualifier eq tree.qualifier) && (name == tree.name) => tree
        case _ => finalize(tree, untpd.Select(qualifier, name))
      }
      def This(tree: Tree, qual: TypeName): This = tree match {
        case tree: This if (qual == tree.qual) => tree
        case _ => finalize(tree, untpd.This(qual))
      }
      def Super(tree: Tree, qual: Tree, mix: TypeName): Super = tree match {
        case tree: Super if (qual eq tree.qual) && (mix == tree.mix) => tree
        case _ => finalize(tree, untpd.Super(qual, mix))
      }
      def Apply(tree: Tree, fun: Tree, args: List[Tree]): Apply = tree match {
        case tree: Apply if (fun eq tree.fun) && (args eq tree.args) => tree
        case _ => finalize(tree, untpd.Apply(fun, args))
      }
      def TypeApply(tree: Tree, fun: Tree, args: List[Tree]): TypeApply = tree match {
        case tree: TypeApply if (fun eq tree.fun) && (args eq tree.args) => tree
        case _ => finalize(tree, untpd.TypeApply(fun, args))
      }
      def Literal(tree: Tree, const: Constant): Literal = tree match {
        case tree: Literal if (const == tree.const) => tree
        case _ => finalize(tree, untpd.Literal(const))
      }
      def New(tree: Tree, tpt: Tree): New = tree match {
        case tree: New if (tpt eq tree.tpt) => tree
        case _ => finalize(tree, untpd.New(tpt))
      }
      def Pair(tree: Tree, left: Tree, right: Tree): Pair = tree match {
        case tree: Pair if (left eq tree.left) && (right eq tree.right) => tree
        case _ => finalize(tree, untpd.Pair(left, right))
      }
      def Typed(tree: Tree, expr: Tree, tpt: Tree): Typed = tree match {
        case tree: Typed if (expr eq tree.expr) && (tpt eq tree.tpt) => tree
        case _ => finalize(tree, untpd.Typed(expr, tpt))
      }
      def NamedArg(tree: Tree, name: Name, arg: Tree): NamedArg = tree match {
        case tree: NamedArg if (name == tree.name) && (arg eq tree.arg) => tree
        case _ => finalize(tree, untpd.NamedArg(name, arg))
      }
      def Assign(tree: Tree, lhs: Tree, rhs: Tree): Assign = tree match {
        case tree: Assign if (lhs eq tree.lhs) && (rhs eq tree.rhs) => tree
        case _ => finalize(tree, untpd.Assign(lhs, rhs))
      }
      def Block(tree: Tree, stats: List[Tree], expr: Tree): Block = tree match {
        case tree: Block if (stats eq tree.stats) && (expr eq tree.expr) => tree
        case _ => finalize(tree, untpd.Block(stats, expr))
      }
      def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree): If = tree match {
        case tree: If if (cond eq tree.cond) && (thenp eq tree.thenp) && (elsep eq tree.elsep) => tree
        case _ => finalize(tree, untpd.If(cond, thenp, elsep))
      }
      def Closure(tree: Tree, env: List[Tree], meth: Tree, tpt: Tree): Closure = tree match {
        case tree: Closure if (env eq tree.env) && (meth eq tree.meth) && (tpt eq tree.tpt) => tree
        case _ => finalize(tree, untpd.Closure(env, meth, tpt))
      }
      def Match(tree: Tree, selector: Tree, cases: List[CaseDef]): Match = tree match {
        case tree: Match if (selector eq tree.selector) && (cases eq tree.cases) => tree
        case _ => finalize(tree, untpd.Match(selector, cases))
      }
      def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree): CaseDef = tree match {
        case tree: CaseDef if (pat eq tree.pat) && (guard eq tree.guard) && (body eq tree.body) => tree
        case _ => finalize(tree, untpd.CaseDef(pat, guard, body))
      }
      def Return(tree: Tree, expr: Tree, from: Tree): Return = tree match {
        case tree: Return if (expr eq tree.expr) && (from eq tree.from) => tree
        case _ => finalize(tree, untpd.Return(expr, from))
      }
      def Try(tree: Tree, expr: Tree, handler: Tree, finalizer: Tree): Try = tree match {
        case tree: Try if (expr eq tree.expr) && (handler eq tree.handler) && (finalizer eq tree.finalizer) => tree
        case _ => finalize(tree, untpd.Try(expr, handler, finalizer))
      }
      def Throw(tree: Tree, expr: Tree): Throw = tree match {
        case tree: Throw if (expr eq tree.expr) => tree
        case _ => finalize(tree, untpd.Throw(expr))
      }
      def SeqLiteral(tree: Tree, elems: List[Tree]): SeqLiteral = tree match {
        case tree: JavaSeqLiteral =>
          if (elems eq tree.elems) tree
          else finalize(tree, new JavaSeqLiteral(elems))
        case tree: SeqLiteral if (elems eq tree.elems) => tree
        case _ => finalize(tree, untpd.SeqLiteral(elems))
      }
      def TypeTree(tree: Tree, original: Tree): TypeTree = tree match {
        case tree: TypeTree if original eq tree.original => tree
        case _ => finalize(tree, untpd.TypeTree(original))
      }
      def SingletonTypeTree(tree: Tree, ref: Tree): SingletonTypeTree = tree match {
        case tree: SingletonTypeTree if (ref eq tree.ref) => tree
        case _ => finalize(tree, untpd.SingletonTypeTree(ref))
      }
      def SelectFromTypeTree(tree: Tree, qualifier: Tree, name: Name): SelectFromTypeTree = tree match {
        case tree: SelectFromTypeTree if (qualifier eq tree.qualifier) && (name == tree.name) => tree
        case _ => finalize(tree, untpd.SelectFromTypeTree(qualifier, name))
      }
      def AndTypeTree(tree: Tree, left: Tree, right: Tree): AndTypeTree = tree match {
        case tree: AndTypeTree if (left eq tree.left) && (right eq tree.right) => tree
        case _ => finalize(tree, untpd.AndTypeTree(left, right))
      }
      def OrTypeTree(tree: Tree, left: Tree, right: Tree): OrTypeTree = tree match {
        case tree: OrTypeTree if (left eq tree.left) && (right eq tree.right) => tree
        case _ => finalize(tree, untpd.OrTypeTree(left, right))
      }
      def RefinedTypeTree(tree: Tree, tpt: Tree, refinements: List[Tree]): RefinedTypeTree = tree match {
        case tree: RefinedTypeTree if (tpt eq tree.tpt) && (refinements eq tree.refinements) => tree
        case _ => finalize(tree, untpd.RefinedTypeTree(tpt, refinements))
      }
      def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]): AppliedTypeTree = tree match {
        case tree: AppliedTypeTree if (tpt eq tree.tpt) && (args eq tree.args) => tree
        case _ => finalize(tree, untpd.AppliedTypeTree(tpt, args))
      }
      def ByNameTypeTree(tree: Tree, result: Tree): ByNameTypeTree = tree match {
        case tree: ByNameTypeTree if (result eq tree.result) => tree
        case _ => finalize(tree, untpd.ByNameTypeTree(result))
      }
      def TypeBoundsTree(tree: Tree, lo: Tree, hi: Tree): TypeBoundsTree = tree match {
        case tree: TypeBoundsTree if (lo eq tree.lo) && (hi eq tree.hi) => tree
        case _ => finalize(tree, untpd.TypeBoundsTree(lo, hi))
      }
      def Bind(tree: Tree, name: Name, body: Tree): Bind = tree match {
        case tree: Bind if (name eq tree.name) && (body eq tree.body) => tree
        case _ => finalize(tree, untpd.Bind(name, body))
      }
      def Alternative(tree: Tree, trees: List[Tree]): Alternative = tree match {
        case tree: Alternative if (trees eq tree.trees) => tree
        case _ => finalize(tree, untpd.Alternative(trees))
      }
      def UnApply(tree: Tree, fun: Tree, implicits: List[Tree], patterns: List[Tree]): UnApply = tree match {
        case tree: UnApply if (fun eq tree.fun) && (implicits eq tree.implicits) && (patterns eq tree.patterns) => tree
        case _ => finalize(tree, untpd.UnApply(fun, implicits, patterns))
      }
      def ValDef(tree: Tree, mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree): ValDef = tree match {
        case tree: ValDef if (mods == tree.mods) && (name == tree.name) && (tpt eq tree.tpt) && (rhs eq tree.rhs) => tree
        case _ => finalize(tree, untpd.ValDef(mods, name, tpt, rhs))
      }
      def DefDef(tree: Tree, mods: Modifiers, name: TermName, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): DefDef = tree match {
        case tree: DefDef if (mods == tree.mods) && (name == tree.name) && (tparams eq tree.tparams) && (vparamss eq tree.vparamss) && (tpt eq tree.tpt) && (rhs eq tree.rhs) => tree
        case _ => finalize(tree, untpd.DefDef(mods, name, tparams, vparamss, tpt, rhs))
      }
      def TypeDef(tree: Tree, mods: Modifiers, name: TypeName, rhs: Tree, tparams: List[untpd.TypeDef] = Nil): TypeDef = tree match {
        case tree: TypeDef if (mods == tree.mods) && (name == tree.name) && (rhs eq tree.rhs) && (tparams eq tree.tparams) => tree
        case _ => finalize(tree, untpd.TypeDef(mods, name, tparams, rhs))
      }
      def Template(tree: Tree, constr: DefDef, parents: List[Tree], self: ValDef, body: List[Tree]): Template = tree match {
        case tree: Template if (constr eq tree.constr) && (parents eq tree.parents) && (self eq tree.self) && (body eq tree.body) => tree
        case _ => finalize(tree, untpd.Template(constr, parents, self, body))
      }
      def Import(tree: Tree, expr: Tree, selectors: List[untpd.Tree]): Import = tree match {
        case tree: Import if (expr eq tree.expr) && (selectors eq tree.selectors) => tree
        case _ => finalize(tree, untpd.Import(expr, selectors))
      }
      def PackageDef(tree: Tree, pid: RefTree, stats: List[Tree]): PackageDef = tree match {
        case tree: PackageDef if (pid eq tree.pid) && (stats eq tree.stats) => tree
        case _ => finalize(tree, untpd.PackageDef(pid, stats))
      }
      def Annotated(tree: Tree, annot: Tree, arg: Tree): Annotated = tree match {
        case tree: Annotated if (annot eq tree.annot) && (arg eq tree.arg) => tree
        case _ => finalize(tree, untpd.Annotated(annot, arg))
      }
      def Thicket(tree: Tree, trees: List[Tree]): Thicket = tree match {
        case tree: Thicket if (trees eq tree.trees) => tree
        case _ => finalize(tree, untpd.Thicket(trees))
      }
    }

    abstract class TreeMap(val cpy: TreeCopier = inst.cpy) {

      def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
        case Ident(name) =>
          tree
        case Select(qualifier, name) =>
          cpy.Select(tree, transform(qualifier), name)
        case This(qual) =>
          tree
        case Super(qual, mix) =>
          cpy.Super(tree, transform(qual), mix)
        case Apply(fun, args) =>
          cpy.Apply(tree, transform(fun), transform(args))
        case TypeApply(fun, args) =>
          cpy.TypeApply(tree, transform(fun), transform(args))
        case Literal(const) =>
          tree
        case New(tpt) =>
          cpy.New(tree, transform(tpt))
        case Pair(left, right) =>
          cpy.Pair(tree, transform(left), transform(right))
        case Typed(expr, tpt) =>
          cpy.Typed(tree, transform(expr), transform(tpt))
        case NamedArg(name, arg) =>
          cpy.NamedArg(tree, name, transform(arg))
        case Assign(lhs, rhs) =>
          cpy.Assign(tree, transform(lhs), transform(rhs))
        case Block(stats, expr) =>
          cpy.Block(tree, transformStats(stats), transform(expr))
        case If(cond, thenp, elsep) =>
          cpy.If(tree, transform(cond), transform(thenp), transform(elsep))
        case Closure(env, meth, tpt) =>
          cpy.Closure(tree, transform(env), transform(meth), transform(tpt))
        case Match(selector, cases) =>
          cpy.Match(tree, transform(selector), transformSub(cases))
        case CaseDef(pat, guard, body) =>
          cpy.CaseDef(tree, transform(pat), transform(guard), transform(body))
        case Return(expr, from) =>
          cpy.Return(tree, transform(expr), transformSub(from))
        case Try(block, handler, finalizer) =>
          cpy.Try(tree, transform(block), transform(handler), transform(finalizer))
        case Throw(expr) =>
          cpy.Throw(tree, transform(expr))
        case SeqLiteral(elems) =>
          cpy.SeqLiteral(tree, transform(elems))
        case TypeTree(original) =>
          tree
        case SingletonTypeTree(ref) =>
          cpy.SingletonTypeTree(tree, transform(ref))
        case SelectFromTypeTree(qualifier, name) =>
          cpy.SelectFromTypeTree(tree, transform(qualifier), name)
        case AndTypeTree(left, right) =>
          cpy.AndTypeTree(tree, transform(left), transform(right))
        case OrTypeTree(left, right) =>
          cpy.OrTypeTree(tree, transform(left), transform(right))
        case RefinedTypeTree(tpt, refinements) =>
          cpy.RefinedTypeTree(tree, transform(tpt), transformSub(refinements))
        case AppliedTypeTree(tpt, args) =>
          cpy.AppliedTypeTree(tree, transform(tpt), transform(args))
        case ByNameTypeTree(result) =>
          cpy.ByNameTypeTree(tree, transform(result))
        case TypeBoundsTree(lo, hi) =>
          cpy.TypeBoundsTree(tree, transform(lo), transform(hi))
        case Bind(name, body) =>
          cpy.Bind(tree, name, transform(body))
        case Alternative(trees) =>
          cpy.Alternative(tree, transform(trees))
        case UnApply(fun, implicits, patterns) =>
          cpy.UnApply(tree, transform(fun), transform(implicits), transform(patterns))
        case EmptyValDef =>
          tree
        case ValDef(mods, name, tpt, rhs) =>
          cpy.ValDef(tree, mods, name, transform(tpt), transform(rhs))
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          cpy.DefDef(tree, mods, name, transformSub(tparams), vparamss mapConserve (transformSub(_)), transform(tpt), transform(rhs))
        case tree @ TypeDef(mods, name, rhs) =>
          cpy.TypeDef(tree, mods, name, transform(rhs), tree.tparams)
        case Template(constr, parents, self, body) =>
          cpy.Template(tree, transformSub(constr), transform(parents), transformSub(self), transformStats(body))
        case Import(expr, selectors) =>
          cpy.Import(tree, transform(expr), selectors)
        case PackageDef(pid, stats) =>
          cpy.PackageDef(tree, transformSub(pid), transformStats(stats))
        case Annotated(annot, arg) =>
          cpy.Annotated(tree, transform(annot), transform(arg))
        case Thicket(trees) =>
          val trees1 = transform(trees)
          if (trees1 eq trees) tree else Thicket(trees1)
      }
      def transformStats(trees: List[Tree])(implicit ctx: Context): List[Tree] =
        transform(trees)
      def transform(trees: List[Tree])(implicit ctx: Context): List[Tree] =
        flatten(trees mapConserve (transform(_)))
      def transformSub[Tr <: Tree](tree: Tr)(implicit ctx: Context): Tr =
        transform(tree).asInstanceOf[Tr]
      def transformSub[Tr <: Tree](trees: List[Tr])(implicit ctx: Context): List[Tr] =
        transform(trees).asInstanceOf[List[Tr]]
    }

    abstract class TreeAccumulator[X] extends ((X, Tree) => X) {
      def apply(x: X, tree: Tree): X
      def apply(x: X, trees: Traversable[Tree]): X = (x /: trees)(apply)
      def foldOver(x: X, tree: Tree): X = tree match {
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
        case Closure(env, meth, tpt) =>
          this(this(this(x, env), meth), tpt)
        case Match(selector, cases) =>
          this(this(x, selector), cases)
        case CaseDef(pat, guard, body) =>
          this(this(this(x, pat), guard), body)
        case Return(expr, from) =>
          this(this(x, expr), from)
        case Try(block, handler, finalizer) =>
          this(this(this(x, block), handler), finalizer)
        case Throw(expr) =>
          this(x, expr)
        case SeqLiteral(elems) =>
          this(x, elems)
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
        case RefinedTypeTree(tpt, refinements) =>
          this(this(x, tpt), refinements)
        case AppliedTypeTree(tpt, args) =>
          this(this(x, tpt), args)
        case ByNameTypeTree(result) =>
          this(x, result)
        case TypeBoundsTree(lo, hi) =>
          this(this(x, lo), hi)
        case Bind(name, body) =>
          this(x, body)
        case Alternative(trees) =>
          this(x, trees)
        case UnApply(fun, implicits, patterns) =>
          this(this(this(x, fun), implicits), patterns)
        case ValDef(mods, name, tpt, rhs) =>
          this(this(x, tpt), rhs)
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          this(this((this(x, tparams) /: vparamss)(apply), tpt), rhs)
        case TypeDef(mods, name, rhs) =>
          this(x, rhs)
        case Template(constr, parents, self, body) =>
          this(this(this(this(x, constr), parents), self), body)
        case Import(expr, selectors) =>
          this(x, expr)
        case PackageDef(pid, stats) =>
          this(this(x, pid), stats)
        case Annotated(annot, arg) =>
          this(this(x, annot), arg)
        case Thicket(ts) =>
          this(x, ts)
      }
    }

    abstract class TreeTraverser extends TreeAccumulator[Unit] {
      def traverse(tree: Tree): Unit
      def apply(x: Unit, tree: Tree) = traverse(tree)
    }

    /** Fold `f` over all tree nodes, in depth-first, prefix order */
    class DeepFolder[X](f: (X, Tree) => X) extends TreeAccumulator[X] {
      def apply(x: X, tree: Tree): X = foldOver(f(x, tree), tree)
    }

    /** Fold `f` over all tree nodes, in depth-first, prefix order, but don't visit
     *  subtrees where `f` returns a different result for the root, i.e. `f(x, root) ne x`.
     */
    class ShallowFolder[X](f: (X, Tree) => X) extends TreeAccumulator[X] {
      def apply(x: X, tree: Tree): X = {
        val x1 = f(x, tree)
        if (x1.asInstanceOf[AnyRef] ne x1.asInstanceOf[AnyRef]) x1
        else foldOver(x1, tree)
      }
    }
  }
}
  // ----- Helper functions and classes ---------------------------------------
/*

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
      case Closure(env, meth, tpt) =>
        finishClosure(tree.derivedClosure(transform(env, c), transformSub(meth, c)), tree, c, plugins)
      case Match(selector, cases) =>
        finishMatch(tree.derivedMatch(transform(selector, c), transformSub(cases, c)), tree, c, plugins)
      case CaseDef(pat, guard, body) =>
        finishCaseDef(tree.derivedCaseDef(transform(pat, c), transform(guard, c), transform(body, c)), tree, c, plugins)
      case Return(expr, from) =>
        finishReturn(tree.derivedReturn(transform(expr, c), transform(from, c)), tree, c, plugins)
      case Try(block, handler, finalizer) =>
        finishTry(tree.derivedTry(transform(block, c), transform(handler, c), transform(finalizer, c)), tree, c, plugins)
      case Throw(expr) =>
        finishThrow(tree.derivedThrow(transform(expr, c)), tree, c, plugins)
      case SeqLiteral(elems) =>
        finishSeqLiteral(tree.derivedSeqLiteral(transform(elems, c)), tree, c, plugins)
      case TypeTree(original) =>
        finishTypeTree(tree, tree, c, plugins)
      case SingletonTypeTree(ref) =>
        finishSingletonTypeTree(tree.derivedSingletonTypeTree(transform(ref, c)), tree, c, plugins)
      case SelectFromTypeTree(qualifier, name) =>
        finishSelectFromTypeTree(tree.derivedSelectFromTypeTree(transform(qualifier, c), name), tree, c, plugins)
      case AndTypeTree(left, right) =>
        finishAndTypeTree(tree.derivedAndTypeTree(transform(left, c), transform(right, c)), tree, c, plugins)
      case OrTypeTree(left, right) =>
        finishOrTypeTree(tree.derivedOrTypeTree(transform(left, c), transform(right, c)), tree, c, plugins)
      case RefinedTypeTree(tpt, refinements) =>
        finishRefinedTypeTree(tree.derivedRefinedTypeTree(transform(tpt, c), transformSub(refinements, c)), tree, c, plugins)
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
      case tree @ TypeDef(mods, name, rhs) =>
        finishTypeDef(tree.derivedTypeDef(mods, name, transform(rhs, c), tree.tparams), tree, c, plugins)
      case Template(constr, parents, self, body) =>
        finishTemplate(tree.derivedTemplate(transformSub(constr, c), transform(parents, c), transformSub(self, c), transform(body, c)), tree, c, plugins)
      case Import(expr, selectors) =>
        finishImport(tree.derivedImport(transform(expr, c), selectors), tree, c, plugins)
      case PackageDef(pid, stats) =>
        finishPackageDef(tree.derivedPackageDef(transformSub(pid, c), transform(stats, c)), tree, c, plugins)
      case Annotated(annot, arg) =>
        finishAnnotated(tree.derivedAnnotated(transform(annot, c), transform(arg, c)), tree, c, plugins)
      case EmptyTree =>
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
      flatten(trees) mapConserve (transform(_, c))
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
    def finishClosure(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
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
    def finishRefinedTypeTree(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishAppliedTypeTree(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishTypeBoundsTree(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishBind(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishAlternative(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishUnApply(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishValDef(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishDefDef(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishTypeDef(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishTemplate(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishImport(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishPackageDef(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishAnnotated(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishEmptyTree(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
    def finishSharedTree(tree: Tree[T], old: Tree[T], c: C, plugins: Plugins) = tree
  }
*/

