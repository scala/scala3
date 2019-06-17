package dotty.tools
package dotc
package ast

import core._
import Types._, Contexts._, Constants._, Names._, Flags._
import Symbols._, StdNames._, Trees._
import util.{Property, SourceFile, NoSource}
import language.higherKinds
import annotation.constructorOnly
import annotation.internal.sharable

object untpd extends Trees.Instance[Untyped] with UntypedTreeInfo {

  // ----- Tree cases that exist in untyped form only ------------------

  abstract class OpTree(implicit @constructorOnly src: SourceFile) extends Tree {
    def op: Ident
    override def isTerm: Boolean = op.isTerm
    override def isType: Boolean = op.isType
  }

  /** A typed subtree of an untyped tree needs to be wrapped in a TypedSplice
   *  @param owner  The current owner at the time the tree was defined
   */
  abstract case class TypedSplice(splice: tpd.Tree)(val owner: Symbol)(implicit @constructorOnly src: SourceFile) extends ProxyTree {
    def forwardTo: tpd.Tree = splice
  }

  object TypedSplice {
    def apply(tree: tpd.Tree)(implicit ctx: Context): TypedSplice =
      new TypedSplice(tree)(ctx.owner) {}
  }

  /** mods object name impl */
  case class ModuleDef(name: TermName, impl: Template)(implicit @constructorOnly src: SourceFile)
    extends MemberDef {
    type ThisTree[-T >: Untyped] <: Trees.NameTree[T] with Trees.MemberDef[T] with ModuleDef
    def withName(name: Name)(implicit ctx: Context): ModuleDef = cpy.ModuleDef(this)(name.toTermName, impl)
  }

  /** An untyped template with a derives clause. Derived parents are added to the end
   *  of the `parents` list. `derivedCount` keeps track of how many there are.
   *  This representation was chosen because it balances two concerns:
   *   - maximize overlap between DerivingTemplate and Template for code streamlining
   *   - keep invariant that elements of untyped trees align with source positions
   */
  class DerivingTemplate(constr: DefDef, parentsOrDerived: List[Tree], self: ValDef, preBody: LazyTreeList, derivedCount: Int)(implicit @constructorOnly src: SourceFile)
  extends Template(constr, parentsOrDerived, self, preBody) {
    override val parents = parentsOrDerived.dropRight(derivedCount)
    override val derived = parentsOrDerived.takeRight(derivedCount)
  }

  case class ParsedTry(expr: Tree, handler: Tree, finalizer: Tree)(implicit @constructorOnly src: SourceFile) extends TermTree

  case class SymbolLit(str: String)(implicit @constructorOnly src: SourceFile) extends TermTree

  /** An interpolated string
   *  @param segments  a list of two element tickets consisting of string literal and argument tree,
   *                   possibly with a simple string literal as last element of the list
   */
  case class InterpolatedString(id: TermName, segments: List[Tree])(implicit @constructorOnly src: SourceFile)
    extends TermTree

  /** A function type */
  case class Function(args: List[Tree], body: Tree)(implicit @constructorOnly src: SourceFile) extends Tree {
    override def isTerm: Boolean = body.isTerm
    override def isType: Boolean = body.isType
  }

  /** A function type with `implicit`, `erased`, or `given` modifiers */
  class FunctionWithMods(args: List[Tree], body: Tree, val mods: Modifiers)(implicit @constructorOnly src: SourceFile)
    extends Function(args, body)

  /** A polymorphic function type */
  case class PolyFunction(targs: List[Tree], body: Tree)(implicit @constructorOnly src: SourceFile) extends Tree {
    override def isTerm = body.isTerm
    override def isType = body.isType
  }

  /** A function created from a wildcard expression
   *  @param  placeholderParams  a list of definitions of synthetic parameters.
   *  @param  body               the function body where wildcards are replaced by
   *                             references to synthetic parameters.
   *  This is equivalent to Function, except that forms a special case for the overlapping
   *  positions tests.
   */
  class WildcardFunction(placeholderParams: List[ValDef], body: Tree)(implicit @constructorOnly src: SourceFile)
    extends Function(placeholderParams, body)

  case class InfixOp(left: Tree, op: Ident, right: Tree)(implicit @constructorOnly src: SourceFile) extends OpTree
  case class PostfixOp(od: Tree, op: Ident)(implicit @constructorOnly src: SourceFile) extends OpTree
  case class PrefixOp(op: Ident, od: Tree)(implicit @constructorOnly src: SourceFile) extends OpTree
  case class Parens(t: Tree)(implicit @constructorOnly src: SourceFile) extends ProxyTree {
    def forwardTo: Tree = t
  }
  case class Tuple(trees: List[Tree])(implicit @constructorOnly src: SourceFile) extends Tree {
    override def isTerm: Boolean = trees.isEmpty || trees.head.isTerm
    override def isType: Boolean = !isTerm
  }
  case class Throw(expr: Tree)(implicit @constructorOnly src: SourceFile) extends TermTree
  case class Quote(quoted: Tree)(implicit @constructorOnly src: SourceFile) extends TermTree
  case class Splice(expr: Tree)(implicit @constructorOnly src: SourceFile) extends TermTree
  case class TypSplice(expr: Tree)(implicit @constructorOnly src: SourceFile) extends TypTree
  case class DoWhile(body: Tree, cond: Tree)(implicit @constructorOnly src: SourceFile) extends TermTree
  case class ForYield(enums: List[Tree], expr: Tree)(implicit @constructorOnly src: SourceFile) extends TermTree
  case class ForDo(enums: List[Tree], body: Tree)(implicit @constructorOnly src: SourceFile) extends TermTree
  case class GenFrom(pat: Tree, expr: Tree, checkMode: GenCheckMode)(implicit @constructorOnly src: SourceFile) extends Tree
  case class GenAlias(pat: Tree, expr: Tree)(implicit @constructorOnly src: SourceFile) extends Tree
  case class ContextBounds(bounds: TypeBoundsTree, cxBounds: List[Tree])(implicit @constructorOnly src: SourceFile) extends TypTree
  case class PatDef(mods: Modifiers, pats: List[Tree], tpt: Tree, rhs: Tree)(implicit @constructorOnly src: SourceFile) extends DefTree
  case class Export(impliedOnly: Boolean, expr: Tree, selectors: List[Tree])(implicit @constructorOnly src: SourceFile) extends Tree

  /** Short-lived usage in typer, does not need copy/transform/fold infrastructure */
  case class DependentTypeTree(tp: List[Symbol] => Type)(implicit @constructorOnly src: SourceFile) extends Tree

  @sharable object EmptyTypeIdent extends Ident(tpnme.EMPTY)(NoSource) with WithoutTypeOrPos[Untyped] {
    override def isEmpty: Boolean = true
  }

  /** A block generated by the XML parser, only treated specially by
   *  `Positioned#checkPos` */
  class XMLBlock(stats: List[Tree], expr: Tree)(implicit @constructorOnly src: SourceFile) extends Block(stats, expr)

  /** An enum to control checking or filtering of patterns in GenFrom trees */
  class GenCheckMode(val x: Int) extends AnyVal
  object GenCheckMode {
    val Ignore = new GenCheckMode(0)  // neither filter nor check since filtering was done before
    val Check = new GenCheckMode(1)   // check that pattern is irrefutable
    val FilterNow = new GenCheckMode(2)  // filter out non-matching elements since we are not in -strict
    val FilterAlways = new GenCheckMode(3) // filter out non-matching elements since pattern is prefixed by `case`
  }

  // ----- Modifiers -----------------------------------------------------
  /** Mod is intended to record syntactic information about modifiers, it's
    * NOT a replacement of FlagSet.
    *
    * For any query about semantic information, check `flags` instead.
    */
  sealed abstract class Mod(val flags: FlagSet)(implicit @constructorOnly src: SourceFile)
  extends Positioned

  object Mod {
    case class Private()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Private)

    case class Protected()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Protected)

    case class Var()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Mutable)

    case class Implicit()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Implicit)

    case class Given()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Given)

    case class Erased()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Erased)

    case class Final()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Final)

    case class Sealed()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Sealed)

    case class Opaque()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Opaque)

    case class Override()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Override)

    case class Abstract()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Abstract)

    case class Lazy()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Lazy)

    case class Inline()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Inline)

    case class Implied()(implicit @constructorOnly src: SourceFile) extends Mod(Flags.Implied)
  }

  /** Modifiers and annotations for definitions
   *
   *  @param flags          The set flags
   *  @param privateWithin  If a private or protected has is followed by a
   *                        qualifier [q], the name q, "" as a typename otherwise.
   *  @param annotations    The annotations preceding the modifiers
   */
  case class Modifiers (
    flags: FlagSet = EmptyFlags,
    privateWithin: TypeName = tpnme.EMPTY,
    annotations: List[Tree] = Nil,
    mods: List[Mod] = Nil) {

    def is(fs: FlagSet): Boolean = flags is fs
    def is(fc: FlagConjunction): Boolean = flags is fc
    def is(fc: FlagSet, butNot: FlagSet): Boolean = flags.is(fc, butNot = butNot)

    def | (fs: FlagSet): Modifiers = withFlags(flags | fs)
    def & (fs: FlagSet): Modifiers = withFlags(flags & fs)
    def &~(fs: FlagSet): Modifiers = withFlags(flags &~ fs)

    def toTypeFlags: Modifiers = withFlags(flags.toTypeFlags)
    def toTermFlags: Modifiers = withFlags(flags.toTermFlags)

    def withFlags(flags: FlagSet): Modifiers =
      if (this.flags == flags) this
      else copy(flags = flags)

    def withoutFlags(flags: FlagSet): Modifiers =
      if (this.is(flags))
        Modifiers(this.flags &~ flags, this.privateWithin, this.annotations, this.mods.filterNot(_.flags.is(flags)))
      else this

    def withAddedMod(mod: Mod): Modifiers =
      if (mods.exists(_ eq mod)) this
      else withMods(mods :+ mod)

    /** Modifiers with given list of Mods. It is checked that
     *  all modifiers are already accounted for in `flags` and `privateWithin`.
     */
    def withMods(ms: List[Mod]): Modifiers = {
      if (mods eq ms) this
      else {
        if (ms.nonEmpty)
          for (m <- ms)
            assert(flags.is(m.flags) ||
                   m.isInstanceOf[Mod.Private] && !privateWithin.isEmpty,
                   s"unaccounted modifier: $m in $this when adding $ms")
        copy(mods = ms)
      }
    }

    def withAddedAnnotation(annot: Tree): Modifiers =
      if (annotations.exists(_ eq annot)) this
      else withAnnotations(annotations :+ annot)

    def withAnnotations(annots: List[Tree]): Modifiers =
      if (annots eq annotations) this
      else copy(annotations = annots)

    def withPrivateWithin(pw: TypeName): Modifiers =
      if (pw.isEmpty) this
      else copy(privateWithin = pw)

    def hasFlags: Boolean = flags != EmptyFlags
    def hasAnnotations: Boolean = annotations.nonEmpty
    def hasPrivateWithin: Boolean = privateWithin != tpnme.EMPTY
    def hasMod(cls: Class[_]) = mods.exists(_.getClass == cls)

    private def isEnum = is(Enum, butNot = JavaDefined)

    def isEnumCase: Boolean = isEnum && is(Case)
    def isEnumClass: Boolean = isEnum && !is(Case)
  }

  @sharable val EmptyModifiers: Modifiers = new Modifiers()

  // ----- TypeTrees that refer to other tree's symbols -------------------

  /** A type tree that gets its type from some other tree's symbol. Enters the
   *  type tree in the References attachment of the `from` tree as a side effect.
   */
  abstract class DerivedTypeTree(implicit @constructorOnly src: SourceFile) extends TypeTree {

    private[this] var myWatched: Tree = EmptyTree

    /** The watched tree; used only for printing */
    def watched: Tree = myWatched

    /** Install the derived type tree as a dependency on `original` */
    def watching(original: DefTree): this.type = {
      myWatched = original
      val existing = original.attachmentOrElse(References, Nil)
      original.putAttachment(References, this :: existing)
      this
    }

    /** Install the derived type tree as a dependency on `sym` */
    def watching(sym: Symbol): this.type = withAttachment(OriginalSymbol, sym)

    /** A hook to ensure that all necessary symbols are completed so that
     *  OriginalSymbol attachments are propagated to this tree
     */
    def ensureCompletions(implicit ctx: Context): Unit = ()

    /** The method that computes the tree with the derived type */
    def derivedTree(originalSym: Symbol)(implicit ctx: Context): tpd.Tree
  }

    /** Property key containing TypeTrees whose type is computed
   *  from the symbol in this type. These type trees have marker trees
   *  TypeRefOfSym or InfoOfSym as their originals.
   */
  val References: Property.Key[List[DerivedTypeTree]] = new Property.Key

  /** Property key for TypeTrees marked with TypeRefOfSym or InfoOfSym
   *  which contains the symbol of the original tree from which this
   *  TypeTree is derived.
   */
  val OriginalSymbol: Property.Key[Symbol] = new Property.Key

  /** Property key for contextual Apply trees of the form `fn given arg` */
  val ApplyGiven: Property.StickyKey[Unit] = new Property.StickyKey

  // ------ Creation methods for untyped only -----------------

  def Ident(name: Name)(implicit src: SourceFile): Ident = new Ident(name)
  def BackquotedIdent(name: Name)(implicit src: SourceFile): BackquotedIdent = new BackquotedIdent(name)
  def SearchFailureIdent(name: Name)(implicit src: SourceFile): SearchFailureIdent = new SearchFailureIdent(name)
  def Select(qualifier: Tree, name: Name)(implicit src: SourceFile): Select = new Select(qualifier, name)
  def SelectWithSig(qualifier: Tree, name: Name, sig: Signature)(implicit src: SourceFile): Select = new SelectWithSig(qualifier, name, sig)
  def This(qual: Ident)(implicit src: SourceFile): This = new This(qual)
  def Super(qual: Tree, mix: Ident)(implicit src: SourceFile): Super = new Super(qual, mix)
  def Apply(fun: Tree, args: List[Tree])(implicit src: SourceFile): Apply = new Apply(fun, args)
  def TypeApply(fun: Tree, args: List[Tree])(implicit src: SourceFile): TypeApply = new TypeApply(fun, args)
  def Literal(const: Constant)(implicit src: SourceFile): Literal = new Literal(const)
  def New(tpt: Tree)(implicit src: SourceFile): New = new New(tpt)
  def Typed(expr: Tree, tpt: Tree)(implicit src: SourceFile): Typed = new Typed(expr, tpt)
  def NamedArg(name: Name, arg: Tree)(implicit src: SourceFile): NamedArg = new NamedArg(name, arg)
  def Assign(lhs: Tree, rhs: Tree)(implicit src: SourceFile): Assign = new Assign(lhs, rhs)
  def Block(stats: List[Tree], expr: Tree)(implicit src: SourceFile): Block = new Block(stats, expr)
  def If(cond: Tree, thenp: Tree, elsep: Tree)(implicit src: SourceFile): If = new If(cond, thenp, elsep)
  def InlineIf(cond: Tree, thenp: Tree, elsep: Tree)(implicit src: SourceFile): If = new InlineIf(cond, thenp, elsep)
  def Closure(env: List[Tree], meth: Tree, tpt: Tree)(implicit src: SourceFile): Closure = new Closure(env, meth, tpt)
  def Match(selector: Tree, cases: List[CaseDef])(implicit src: SourceFile): Match = new Match(selector, cases)
  def InlineMatch(selector: Tree, cases: List[CaseDef])(implicit src: SourceFile): Match = new InlineMatch(selector, cases)
  def CaseDef(pat: Tree, guard: Tree, body: Tree)(implicit src: SourceFile): CaseDef = new CaseDef(pat, guard, body)
  def Labeled(bind: Bind, expr: Tree)(implicit src: SourceFile): Labeled = new Labeled(bind, expr)
  def Return(expr: Tree, from: Tree)(implicit src: SourceFile): Return = new Return(expr, from)
  def WhileDo(cond: Tree, body: Tree)(implicit src: SourceFile): WhileDo = new WhileDo(cond, body)
  def Try(expr: Tree, cases: List[CaseDef], finalizer: Tree)(implicit src: SourceFile): Try = new Try(expr, cases, finalizer)
  def SeqLiteral(elems: List[Tree], elemtpt: Tree)(implicit src: SourceFile): SeqLiteral = new SeqLiteral(elems, elemtpt)
  def JavaSeqLiteral(elems: List[Tree], elemtpt: Tree)(implicit src: SourceFile): JavaSeqLiteral = new JavaSeqLiteral(elems, elemtpt)
  def Inlined(call: tpd.Tree, bindings: List[MemberDef], expansion: Tree)(implicit src: SourceFile): Inlined = new Inlined(call, bindings, expansion)
  def TypeTree()(implicit src: SourceFile): TypeTree = new TypeTree()
  def SingletonTypeTree(ref: Tree)(implicit src: SourceFile): SingletonTypeTree = new SingletonTypeTree(ref)
  def RefinedTypeTree(tpt: Tree, refinements: List[Tree])(implicit src: SourceFile): RefinedTypeTree = new RefinedTypeTree(tpt, refinements)
  def AppliedTypeTree(tpt: Tree, args: List[Tree])(implicit src: SourceFile): AppliedTypeTree = new AppliedTypeTree(tpt, args)
  def LambdaTypeTree(tparams: List[TypeDef], body: Tree)(implicit src: SourceFile): LambdaTypeTree = new LambdaTypeTree(tparams, body)
  def MatchTypeTree(bound: Tree, selector: Tree, cases: List[CaseDef])(implicit src: SourceFile): MatchTypeTree = new MatchTypeTree(bound, selector, cases)
  def ByNameTypeTree(result: Tree)(implicit src: SourceFile): ByNameTypeTree = new ByNameTypeTree(result)
  def TypeBoundsTree(lo: Tree, hi: Tree)(implicit src: SourceFile): TypeBoundsTree = new TypeBoundsTree(lo, hi)
  def Bind(name: Name, body: Tree)(implicit src: SourceFile): Bind = new Bind(name, body)
  def Alternative(trees: List[Tree])(implicit src: SourceFile): Alternative = new Alternative(trees)
  def UnApply(fun: Tree, implicits: List[Tree], patterns: List[Tree])(implicit src: SourceFile): UnApply = new UnApply(fun, implicits, patterns)
  def ValDef(name: TermName, tpt: Tree, rhs: LazyTree)(implicit src: SourceFile): ValDef = new ValDef(name, tpt, rhs)
  def BackquotedValDef(name: TermName, tpt: Tree, rhs: LazyTree)(implicit src: SourceFile): ValDef = new BackquotedValDef(name, tpt, rhs)
  def DefDef(name: TermName, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: LazyTree)(implicit src: SourceFile): DefDef = new DefDef(name, tparams, vparamss, tpt, rhs)
  def BackquotedDefDef(name: TermName, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: LazyTree)(implicit src: SourceFile): DefDef = new BackquotedDefDef(name, tparams, vparamss, tpt, rhs)
  def TypeDef(name: TypeName, rhs: Tree)(implicit src: SourceFile): TypeDef = new TypeDef(name, rhs)
  def Template(constr: DefDef, parents: List[Tree], derived: List[Tree], self: ValDef, body: LazyTreeList)(implicit src: SourceFile): Template =
    if (derived.isEmpty) new Template(constr, parents, self, body)
    else new DerivingTemplate(constr, parents ++ derived, self, body, derived.length)
  def Import(importImplied: Boolean, expr: Tree, selectors: List[Tree])(implicit src: SourceFile): Import = new Import(importImplied, expr, selectors)
  def PackageDef(pid: RefTree, stats: List[Tree])(implicit src: SourceFile): PackageDef = new PackageDef(pid, stats)
  def Annotated(arg: Tree, annot: Tree)(implicit src: SourceFile): Annotated = new Annotated(arg, annot)

  // ------ Additional creation methods for untyped only -----------------

  /**     new T(args1)...(args_n)
   *  ==>
   *      new T.<init>[Ts](args1)...(args_n)
   *
   *  where `Ts` are the class type arguments of `T` or its class type alias.
   *  Note: we also keep any type arguments as parts of `T`. This is necessary to allow
   *  navigation into these arguments from the IDE, and to do the right thing in
   *  PrepareInlineable.
   */
  def New(tpt: Tree, argss: List[List[Tree]])(implicit ctx: Context): Tree =
    ensureApplied((makeNew(tpt) /: argss)(Apply(_, _)))

  /** A new expression with constrictor and possibly type arguments. See
   *  `New(tpt, argss)` for details.
   */
  def makeNew(tpt: Tree)(implicit ctx: Context): Tree = {
    val (tycon, targs) = tpt match {
      case AppliedTypeTree(tycon, targs) =>
        (tycon, targs)
      case TypedSplice(tpt1: tpd.Tree) =>
        val argTypes = tpt1.tpe.dealias.argTypesLo
        def wrap(tpe: Type) = TypeTree(tpe).withSpan(tpt.span)
        (tpt, argTypes.map(wrap))
      case _ =>
        (tpt, Nil)
    }
    val nu: Tree = Select(New(tycon), nme.CONSTRUCTOR)
    if (targs.nonEmpty) TypeApply(nu, targs) else nu
  }

  def Block(stat: Tree, expr: Tree)(implicit src: SourceFile): Block =
    Block(stat :: Nil, expr)

  def Apply(fn: Tree, arg: Tree)(implicit src: SourceFile): Apply =
    Apply(fn, arg :: Nil)

  def ensureApplied(tpt: Tree)(implicit src: SourceFile): Tree = tpt match {
    case _: Apply => tpt
    case _ => Apply(tpt, Nil)
  }

  def AppliedTypeTree(tpt: Tree, arg: Tree)(implicit src: SourceFile): AppliedTypeTree =
    AppliedTypeTree(tpt, arg :: Nil)

  def TypeTree(tpe: Type)(implicit ctx: Context): TypedSplice = TypedSplice(TypeTree().withTypeUnchecked(tpe))

  def unitLiteral(implicit src: SourceFile): Literal = Literal(Constant(()))

  def ref(tp: NamedType)(implicit ctx: Context): Tree =
    TypedSplice(tpd.ref(tp))

  def rootDot(name: Name)(implicit src: SourceFile): Select = Select(Ident(nme.ROOTPKG), name)
  def scalaDot(name: Name)(implicit src: SourceFile): Select = Select(rootDot(nme.scala_), name)
  def scalaUnit(implicit src: SourceFile): Select = scalaDot(tpnme.Unit)
  def scalaAny(implicit src: SourceFile): Select = scalaDot(tpnme.Any)
  def javaDotLangDot(name: Name)(implicit src: SourceFile): Select = Select(Select(Ident(nme.java), nme.lang), name)

  def makeConstructor(tparams: List[TypeDef], vparamss: List[List[ValDef]], rhs: Tree = EmptyTree)(implicit ctx: Context): DefDef =
    DefDef(nme.CONSTRUCTOR, tparams, vparamss, TypeTree(), rhs)

  def emptyConstructor(implicit ctx: Context): DefDef =
    makeConstructor(Nil, Nil)

  def makeSelfDef(name: TermName, tpt: Tree)(implicit ctx: Context): ValDef =
    ValDef(name, tpt, EmptyTree).withFlags(PrivateLocal)

  def makeTupleOrParens(ts: List[Tree])(implicit ctx: Context): Tree = ts match {
    case t :: Nil => Parens(t)
    case _ => Tuple(ts)
  }

  def makeTuple(ts: List[Tree])(implicit ctx: Context): Tree = ts match {
    case t :: Nil => t
    case _ => Tuple(ts)
  }

  def makeAndType(left: Tree, right: Tree)(implicit ctx: Context): AppliedTypeTree =
    AppliedTypeTree(ref(defn.andType.typeRef), left :: right :: Nil)

  def makeParameter(pname: TermName, tpe: Tree, mods: Modifiers = EmptyModifiers, isBackquoted: Boolean = false)(implicit ctx: Context): ValDef = {
    val vdef =
      if (isBackquoted) BackquotedValDef(pname, tpe, EmptyTree)
      else ValDef(pname, tpe, EmptyTree)
    vdef.withMods(mods | Param)
  }

  def makeSyntheticParameter(n: Int = 1, tpt: Tree = null, flags: FlagSet = SyntheticTermParam)(implicit ctx: Context): ValDef =
    ValDef(nme.syntheticParamName(n), if (tpt == null) TypeTree() else tpt, EmptyTree)
      .withFlags(flags)

  def lambdaAbstract(tparams: List[TypeDef], tpt: Tree)(implicit ctx: Context): Tree =
    if (tparams.isEmpty) tpt else LambdaTypeTree(tparams, tpt)

  /** A reference to given definition. If definition is a repeated
   *  parameter, the reference will be a repeated argument.
   */
  def refOfDef(tree: MemberDef)(implicit ctx: Context): Tree = tree match {
    case ValDef(_, PostfixOp(_, Ident(tpnme.raw.STAR)), _) => repeated(Ident(tree.name))
    case _ => Ident(tree.name)
  }

  /** A repeated argument such as `arg: _*` */
  def repeated(arg: Tree)(implicit ctx: Context): Typed = Typed(arg, Ident(tpnme.WILDCARD_STAR))

// ----- Accessing modifiers ----------------------------------------------------

  abstract class ModsDecorator { def mods: Modifiers }

  implicit class modsDeco(val mdef: MemberDef)(implicit ctx: Context) {
    def mods: Modifiers = mdef.rawMods
  }

// --------- Copier/Transformer/Accumulator classes for untyped trees -----

  override val cpy: UntypedTreeCopier = new UntypedTreeCopier

  class UntypedTreeCopier extends TreeCopier {

    def postProcess(tree: Tree, copied: Tree): copied.ThisTree[Untyped] =
      copied.asInstanceOf[copied.ThisTree[Untyped]]

    def postProcess(tree: Tree, copied: MemberDef): copied.ThisTree[Untyped] = {
      tree match {
        case tree: MemberDef => copied.withMods(tree.rawMods)
        case _ => copied
      }
    }.asInstanceOf[copied.ThisTree[Untyped]]

    def ModuleDef(tree: Tree)(name: TermName, impl: Template)(implicit ctx: Context): ModuleDef = tree match {
      case tree: ModuleDef if (name eq tree.name) && (impl eq tree.impl) => tree
      case _ => finalize(tree, untpd.ModuleDef(name, impl)(tree.source))
    }
    def ParsedTry(tree: Tree)(expr: Tree, handler: Tree, finalizer: Tree)(implicit ctx: Context): TermTree = tree match {
      case tree: ParsedTry if (expr eq tree.expr) && (handler eq tree.handler) && (finalizer eq tree.finalizer) => tree
      case _ => finalize(tree, untpd.ParsedTry(expr, handler, finalizer)(tree.source))
    }
    def SymbolLit(tree: Tree)(str: String)(implicit ctx: Context): TermTree = tree match {
      case tree: SymbolLit if str == tree.str => tree
      case _ => finalize(tree, untpd.SymbolLit(str)(tree.source))
    }
    def InterpolatedString(tree: Tree)(id: TermName, segments: List[Tree])(implicit ctx: Context): TermTree = tree match {
      case tree: InterpolatedString if (id eq tree.id) && (segments eq tree.segments) => tree
      case _ => finalize(tree, untpd.InterpolatedString(id, segments)(tree.source))
    }
    def Function(tree: Tree)(args: List[Tree], body: Tree)(implicit ctx: Context): Tree = tree match {
      case tree: Function if (args eq tree.args) && (body eq tree.body) => tree
      case _ => finalize(tree, untpd.Function(args, body)(tree.source))
    }
    def PolyFunction(tree: Tree)(targs: List[Tree], body: Tree)(implicit ctx: Context): Tree = tree match {
      case tree: PolyFunction if (targs eq tree.targs) && (body eq tree.body) => tree
      case _ => finalize(tree, untpd.PolyFunction(targs, body)(tree.source))
    }
    def InfixOp(tree: Tree)(left: Tree, op: Ident, right: Tree)(implicit ctx: Context): Tree = tree match {
      case tree: InfixOp if (left eq tree.left) && (op eq tree.op) && (right eq tree.right) => tree
      case _ => finalize(tree, untpd.InfixOp(left, op, right)(tree.source))
    }
    def PostfixOp(tree: Tree)(od: Tree, op: Ident)(implicit ctx: Context): Tree = tree match {
      case tree: PostfixOp if (od eq tree.od) && (op eq tree.op) => tree
      case _ => finalize(tree, untpd.PostfixOp(od, op)(tree.source))
    }
    def PrefixOp(tree: Tree)(op: Ident, od: Tree)(implicit ctx: Context): Tree = tree match {
      case tree: PrefixOp if (op eq tree.op) && (od eq tree.od) => tree
      case _ => finalize(tree, untpd.PrefixOp(op, od)(tree.source))
    }
    def Parens(tree: Tree)(t: Tree)(implicit ctx: Context): ProxyTree = tree match {
      case tree: Parens if t eq tree.t => tree
      case _ => finalize(tree, untpd.Parens(t)(tree.source))
    }
    def Tuple(tree: Tree)(trees: List[Tree])(implicit ctx: Context): Tree = tree match {
      case tree: Tuple if trees eq tree.trees => tree
      case _ => finalize(tree, untpd.Tuple(trees)(tree.source))
    }
    def Throw(tree: Tree)(expr: Tree)(implicit ctx: Context): TermTree = tree match {
      case tree: Throw if expr eq tree.expr => tree
      case _ => finalize(tree, untpd.Throw(expr)(tree.source))
    }
    def Quote(tree: Tree)(quoted: Tree)(implicit ctx: Context): Tree = tree match {
      case tree: Quote if quoted eq tree.quoted => tree
      case _ => finalize(tree, untpd.Quote(quoted)(tree.source))
    }
    def Splice(tree: Tree)(expr: Tree)(implicit ctx: Context): Tree = tree match {
      case tree: Splice if expr eq tree.expr => tree
      case _ => finalize(tree, untpd.Splice(expr)(tree.source))
    }
    def TypSplice(tree: Tree)(expr: Tree)(implicit ctx: Context): Tree = tree match {
      case tree: TypSplice if expr eq tree.expr => tree
      case _ => finalize(tree, untpd.TypSplice(expr)(tree.source))
    }
    def DoWhile(tree: Tree)(body: Tree, cond: Tree)(implicit ctx: Context): TermTree = tree match {
      case tree: DoWhile if (body eq tree.body) && (cond eq tree.cond) => tree
      case _ => finalize(tree, untpd.DoWhile(body, cond)(tree.source))
    }
    def ForYield(tree: Tree)(enums: List[Tree], expr: Tree)(implicit ctx: Context): TermTree = tree match {
      case tree: ForYield if (enums eq tree.enums) && (expr eq tree.expr) => tree
      case _ => finalize(tree, untpd.ForYield(enums, expr)(tree.source))
    }
    def ForDo(tree: Tree)(enums: List[Tree], body: Tree)(implicit ctx: Context): TermTree = tree match {
      case tree: ForDo if (enums eq tree.enums) && (body eq tree.body) => tree
      case _ => finalize(tree, untpd.ForDo(enums, body)(tree.source))
    }
    def GenFrom(tree: Tree)(pat: Tree, expr: Tree, checkMode: GenCheckMode)(implicit ctx: Context): Tree = tree match {
      case tree: GenFrom if (pat eq tree.pat) && (expr eq tree.expr) && (checkMode == tree.checkMode) => tree
      case _ => finalize(tree, untpd.GenFrom(pat, expr, checkMode)(tree.source))
    }
    def GenAlias(tree: Tree)(pat: Tree, expr: Tree)(implicit ctx: Context): Tree = tree match {
      case tree: GenAlias if (pat eq tree.pat) && (expr eq tree.expr) => tree
      case _ => finalize(tree, untpd.GenAlias(pat, expr)(tree.source))
    }
    def ContextBounds(tree: Tree)(bounds: TypeBoundsTree, cxBounds: List[Tree])(implicit ctx: Context): TypTree = tree match {
      case tree: ContextBounds if (bounds eq tree.bounds) && (cxBounds eq tree.cxBounds) => tree
      case _ => finalize(tree, untpd.ContextBounds(bounds, cxBounds)(tree.source))
    }
    def PatDef(tree: Tree)(mods: Modifiers, pats: List[Tree], tpt: Tree, rhs: Tree)(implicit ctx: Context): Tree = tree match {
      case tree: PatDef if (mods eq tree.mods) && (pats eq tree.pats) && (tpt eq tree.tpt) && (rhs eq tree.rhs) => tree
      case _ => finalize(tree, untpd.PatDef(mods, pats, tpt, rhs)(tree.source))
    }
    def Export(tree: Tree)(impliedOnly: Boolean, expr: Tree, selectors: List[Tree])(implicit ctx: Context): Tree = tree match {
      case tree: Export if (impliedOnly == tree.impliedOnly) && (expr eq tree.expr) && (selectors eq tree.selectors) => tree
      case _ => finalize(tree, untpd.Export(impliedOnly, expr, selectors)(tree.source))
    }
    def TypedSplice(tree: Tree)(splice: tpd.Tree)(implicit ctx: Context): ProxyTree = tree match {
      case tree: TypedSplice if splice `eq` tree.splice => tree
      case _ => finalize(tree, untpd.TypedSplice(splice)(ctx))
    }
  }

  abstract class UntypedTreeMap(cpy: UntypedTreeCopier = untpd.cpy) extends TreeMap(cpy) {
    override def transformMoreCases(tree: Tree)(implicit ctx: Context): Tree = tree match {
      case ModuleDef(name, impl) =>
        cpy.ModuleDef(tree)(name, transformSub(impl))
      case tree: DerivingTemplate =>
        cpy.Template(tree)(transformSub(tree.constr), transform(tree.parents), transform(tree.derived), transformSub(tree.self), transformStats(tree.body))
      case ParsedTry(expr, handler, finalizer) =>
        cpy.ParsedTry(tree)(transform(expr), transform(handler), transform(finalizer))
      case SymbolLit(str) =>
        cpy.SymbolLit(tree)(str)
      case InterpolatedString(id, segments) =>
        cpy.InterpolatedString(tree)(id, segments.mapConserve(transform))
      case Function(args, body) =>
        cpy.Function(tree)(transform(args), transform(body))
      case PolyFunction(targs, body) =>
        cpy.PolyFunction(tree)(transform(targs), transform(body))
      case InfixOp(left, op, right) =>
        cpy.InfixOp(tree)(transform(left), op, transform(right))
      case PostfixOp(od, op) =>
        cpy.PostfixOp(tree)(transform(od), op)
      case PrefixOp(op, od) =>
        cpy.PrefixOp(tree)(op, transform(od))
      case Parens(t) =>
        cpy.Parens(tree)(transform(t))
      case Tuple(trees) =>
        cpy.Tuple(tree)(transform(trees))
      case Throw(expr) =>
        cpy.Throw(tree)(transform(expr))
      case Quote(t) =>
        cpy.Quote(tree)(transform(t))
      case Splice(expr) =>
        cpy.Splice(tree)(transform(expr))
      case TypSplice(expr) =>
        cpy.TypSplice(tree)(transform(expr))
      case DoWhile(body, cond) =>
        cpy.DoWhile(tree)(transform(body), transform(cond))
      case ForYield(enums, expr) =>
        cpy.ForYield(tree)(transform(enums), transform(expr))
      case ForDo(enums, body) =>
        cpy.ForDo(tree)(transform(enums), transform(body))
      case GenFrom(pat, expr, checkMode) =>
        cpy.GenFrom(tree)(transform(pat), transform(expr), checkMode)
      case GenAlias(pat, expr) =>
        cpy.GenAlias(tree)(transform(pat), transform(expr))
      case ContextBounds(bounds, cxBounds) =>
        cpy.ContextBounds(tree)(transformSub(bounds), transform(cxBounds))
      case PatDef(mods, pats, tpt, rhs) =>
        cpy.PatDef(tree)(mods, transform(pats), transform(tpt), transform(rhs))
      case Export(impliedOnly, expr, selectors) =>
        cpy.Export(tree)(impliedOnly, transform(expr), selectors)
      case TypedSplice(_) =>
        tree
      case _ =>
        super.transformMoreCases(tree)
    }
  }

  abstract class UntypedTreeAccumulator[X] extends TreeAccumulator[X] { self =>
    override def foldMoreCases(x: X, tree: Tree)(implicit ctx: Context): X = tree match {
      case ModuleDef(name, impl) =>
        this(x, impl)
      case tree: DerivingTemplate =>
        this(this(this(this(this(x, tree.constr), tree.parents), tree.derived), tree.self), tree.body)
      case ParsedTry(expr, handler, finalizer) =>
        this(this(this(x, expr), handler), finalizer)
      case SymbolLit(str) =>
        x
      case InterpolatedString(id, segments) =>
        this(x, segments)
      case Function(args, body) =>
        this(this(x, args), body)
      case PolyFunction(targs, body) =>
        this(this(x, targs), body)
      case InfixOp(left, op, right) =>
        this(this(this(x, left), op), right)
      case PostfixOp(od, op) =>
        this(this(x, od), op)
      case PrefixOp(op, od) =>
        this(this(x, op), od)
      case Parens(t) =>
        this(x, t)
      case Tuple(trees) =>
        this(x, trees)
      case Throw(expr) =>
        this(x, expr)
      case Quote(t) =>
        this(x, t)
      case Splice(expr) =>
        this(x, expr)
      case TypSplice(expr) =>
        this(x, expr)
      case DoWhile(body, cond) =>
        this(this(x, body), cond)
      case ForYield(enums, expr) =>
        this(this(x, enums), expr)
      case ForDo(enums, body) =>
        this(this(x, enums), body)
      case GenFrom(pat, expr, _) =>
        this(this(x, pat), expr)
      case GenAlias(pat, expr) =>
        this(this(x, pat), expr)
      case ContextBounds(bounds, cxBounds) =>
        this(this(x, bounds), cxBounds)
      case PatDef(mods, pats, tpt, rhs) =>
        this(this(this(x, pats), tpt), rhs)
      case Export(_, expr, _) =>
        this(x, expr)
      case TypedSplice(splice) =>
        this(x, splice)
      case _ =>
        super.foldMoreCases(x, tree)
    }
  }

  abstract class UntypedTreeTraverser extends UntypedTreeAccumulator[Unit] {
    def traverse(tree: Tree)(implicit ctx: Context): Unit
    def apply(x: Unit, tree: Tree)(implicit ctx: Context): Unit = traverse(tree)
    protected def traverseChildren(tree: Tree)(implicit ctx: Context): Unit = foldOver((), tree)
  }

  /** Fold `f` over all tree nodes, in depth-first, prefix order */
  class UntypedDeepFolder[X](f: (X, Tree) => X) extends UntypedTreeAccumulator[X] {
    def apply(x: X, tree: Tree)(implicit ctx: Context): X = foldOver(f(x, tree), tree)
  }
}
