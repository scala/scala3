package dotty.tools
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, NameOps._, Flags._
import Denotations._, SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._
import Decorators._
import util.Attachment
import language.higherKinds
import collection.mutable.ListBuffer

object untpd extends Trees.Instance[Untyped] with UntypedTreeInfo {

  // ----- Tree cases that exist in untyped form only ------------------

  trait OpTree extends Tree {
    def op: Name
    override def isTerm = op.isTermName
    override def isType = op.isTypeName
  }

  /** A typed subtree of an untyped tree needs to be wrapped in a TypedSlice */
  case class TypedSplice(tree: tpd.Tree) extends ProxyTree {
    def forwardTo = tree
  }

  /** mods object name impl */
  case class ModuleDef(name: TermName, impl: Template)
    extends MemberDef {
    type ThisTree[-T >: Untyped] <: Trees.NameTree[T] with Trees.MemberDef[T] with ModuleDef
    def withName(name: Name)(implicit ctx: Context) = cpy.ModuleDef(this)(name.toTermName, impl)
  }

  case class ParsedTry(expr: Tree, handler: Tree, finalizer: Tree) extends TermTree

  case class SymbolLit(str: String) extends TermTree
  case class InterpolatedString(id: TermName, strings: List[Literal], elems: List[Tree]) extends TermTree
  case class Function(args: List[Tree], body: Tree) extends Tree {
    override def isTerm = body.isTerm
    override def isType = body.isType
  }
  case class InfixOp(left: Tree, op: Name, right: Tree) extends OpTree
  case class PostfixOp(od: Tree, op: Name) extends OpTree
  case class PrefixOp(op: Name, od: Tree) extends OpTree
  case class Parens(t: Tree) extends ProxyTree {
    def forwardTo = t
  }
  case class Tuple(trees: List[Tree]) extends Tree {
    override def isTerm = trees.isEmpty || trees.head.isTerm
    override def isType = !isTerm
  }
  case class Throw(expr: Tree) extends TermTree
  case class WhileDo(cond: Tree, body: Tree) extends TermTree
  case class DoWhile(body: Tree, cond: Tree) extends TermTree
  case class ForYield(enums: List[Tree], expr: Tree) extends TermTree
  case class ForDo(enums: List[Tree], body: Tree) extends TermTree
  case class GenFrom(pat: Tree, expr: Tree) extends Tree
  case class GenAlias(pat: Tree, expr: Tree) extends Tree
  case class ContextBounds(bounds: TypeBoundsTree, cxBounds: List[Tree]) extends TypTree
  case class PatDef(mods: Modifiers, pats: List[Tree], tpt: Tree, rhs: Tree) extends DefTree

  class PolyTypeDef(name: TypeName, override val tparams: List[TypeDef], rhs: Tree)
    extends TypeDef(name, rhs)

  // ----- TypeTrees that refer to other tree's symbols -------------------

  /** A type tree that gets its type from some other tree's symbol. Enters the
   *  type tree in the References attachment of the `from` tree as a side effect.
   */
  abstract class DerivedTypeTree extends TypeTree(EmptyTree) {

    private var myWatched: Tree = EmptyTree

    /** The watched tree; used only for printing */
    def watched: Tree = myWatched

    /** Install the derived type tree as a dependency on `original` */
    def watching(original: DefTree): this.type = {
      myWatched = original
      val existing = original.attachmentOrElse(References, Nil)
      original.putAttachment(References, this :: existing)
      this
    }

    /** A hook to ensure that all necessary symbols are completed so that
     *  OriginalSymbol attachments are propagated to this tree
     */
    def ensureCompletions(implicit ctx: Context): Unit = ()

    /** The method that computes the type of this tree */
    def derivedType(originalSym: Symbol)(implicit ctx: Context): Type
  }

    /** Attachment key containing TypeTrees whose type is computed
   *  from the symbol in this type. These type trees have marker trees
   *  TypeRefOfSym or InfoOfSym as their originals.
   */
  val References = new Attachment.Key[List[Tree]]

  /** Attachment key for TypeTrees marked with TypeRefOfSym or InfoOfSym
   *  which contains the symbol of the original tree from which this
   *  TypeTree is derived.
   */
  val OriginalSymbol = new Attachment.Key[Symbol]

  // ------ Creation methods for untyped only -----------------

  def Ident(name: Name): Ident = new Ident(name)
  def BackquotedIdent(name: Name): BackquotedIdent = new BackquotedIdent(name)
  def Select(qualifier: Tree, name: Name): Select = new Select(qualifier, name)
  def SelectWithSig(qualifier: Tree, name: Name, sig: Signature): Select = new SelectWithSig(qualifier, name, sig)
  def This(qual: TypeName): This = new This(qual)
  def Super(qual: Tree, mix: TypeName): Super = new Super(qual, mix)
  def Apply(fun: Tree, args: List[Tree]): Apply = new Apply(fun, args)
  def TypeApply(fun: Tree, args: List[Tree]): TypeApply = new TypeApply(fun, args)
  def Literal(const: Constant): Literal = new Literal(const)
  def New(tpt: Tree): New = new New(tpt)
  def Pair(left: Tree, right: Tree): Pair = new Pair(left, right)
  def Typed(expr: Tree, tpt: Tree): Typed = new Typed(expr, tpt)
  def NamedArg(name: Name, arg: Tree): NamedArg = new NamedArg(name, arg)
  def Assign(lhs: Tree, rhs: Tree): Assign = new Assign(lhs, rhs)
  def Block(stats: List[Tree], expr: Tree): Block = new Block(stats, expr)
  def If(cond: Tree, thenp: Tree, elsep: Tree): If = new If(cond, thenp, elsep)
  def Closure(env: List[Tree], meth: Tree, tpt: Tree): Closure = new Closure(env, meth, tpt)
  def Match(selector: Tree, cases: List[CaseDef]): Match = new Match(selector, cases)
  def CaseDef(pat: Tree, guard: Tree, body: Tree): CaseDef = new CaseDef(pat, guard, body)
  def Return(expr: Tree, from: Tree): Return = new Return(expr, from)
  def Try(expr: Tree, cases: List[CaseDef], finalizer: Tree): Try = new Try(expr, cases, finalizer)
  def SeqLiteral(elems: List[Tree], elemtpt: Tree): SeqLiteral = new SeqLiteral(elems, elemtpt)
  def JavaSeqLiteral(elems: List[Tree], elemtpt: Tree): JavaSeqLiteral = new JavaSeqLiteral(elems, elemtpt)
  def TypeTree(original: Tree): TypeTree = new TypeTree(original)
  def TypeTree() = new TypeTree(EmptyTree)
  def SingletonTypeTree(ref: Tree): SingletonTypeTree = new SingletonTypeTree(ref)
  def SelectFromTypeTree(qualifier: Tree, name: Name): SelectFromTypeTree = new SelectFromTypeTree(qualifier, name)
  def AndTypeTree(left: Tree, right: Tree): AndTypeTree = new AndTypeTree(left, right)
  def OrTypeTree(left: Tree, right: Tree): OrTypeTree = new OrTypeTree(left, right)
  def RefinedTypeTree(tpt: Tree, refinements: List[Tree]): RefinedTypeTree = new RefinedTypeTree(tpt, refinements)
  def AppliedTypeTree(tpt: Tree, args: List[Tree]): AppliedTypeTree = new AppliedTypeTree(tpt, args)
  def TypeLambdaTree(tparams: List[TypeDef], body: Tree): TypeLambdaTree = new TypeLambdaTree(tparams, body)
  def ByNameTypeTree(result: Tree): ByNameTypeTree = new ByNameTypeTree(result)
  def TypeBoundsTree(lo: Tree, hi: Tree): TypeBoundsTree = new TypeBoundsTree(lo, hi)
  def Bind(name: Name, body: Tree): Bind = new Bind(name, body)
  def Alternative(trees: List[Tree]): Alternative = new Alternative(trees)
  def UnApply(fun: Tree, implicits: List[Tree], patterns: List[Tree]): UnApply = new UnApply(fun, implicits, patterns)
  def ValDef(name: TermName, tpt: Tree, rhs: LazyTree): ValDef = new ValDef(name, tpt, rhs)
  def DefDef(name: TermName, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: LazyTree): DefDef = new DefDef(name, tparams, vparamss, tpt, rhs)
  def TypeDef(name: TypeName, rhs: Tree): TypeDef = new TypeDef(name, rhs)
  def Template(constr: DefDef, parents: List[Tree], self: ValDef, body: LazyTreeList): Template = new Template(constr, parents, self, body)
  def Import(expr: Tree, selectors: List[untpd.Tree]): Import = new Import(expr, selectors)
  def PackageDef(pid: RefTree, stats: List[Tree]): PackageDef = new PackageDef(pid, stats)
  def Annotated(annot: Tree, arg: Tree): Annotated = new Annotated(annot, arg)

  // ------ Additional creation methods for untyped only -----------------

  // def TypeTree(tpe: Type): TypeTree = TypeTree().withType(tpe) todo: move to untpd/tpd

  /**     new pre.C[Ts](args1)...(args_n)
   *  ==>
   *      (new pre.C).<init>[Ts](args1)...(args_n)
   */
  def New(tpt: Tree, argss: List[List[Tree]])(implicit ctx: Context): Tree = {
    val (tycon, targs) = tpt match {
      case AppliedTypeTree(tycon, targs) =>
        (tycon, targs)
      case TypedSplice(AppliedTypeTree(tycon, targs)) =>
        (TypedSplice(tycon), targs map TypedSplice)
      case TypedSplice(tpt1: Tree) =>
        val argTypes = tpt1.tpe.argTypes
        val tycon = tpt1.tpe.withoutArgs(argTypes)
        def wrap(tpe: Type) = TypeTree(tpe) withPos tpt.pos
        (wrap(tycon), argTypes map wrap)
      case _ =>
        (tpt, Nil)
    }
    var prefix: Tree = Select(New(tycon), nme.CONSTRUCTOR)
    if (targs.nonEmpty) prefix = TypeApply(prefix, targs)
    ensureApplied((prefix /: argss)(Apply(_, _)))
  }

  def Block(stat: Tree, expr: Tree): Block =
    Block(stat :: Nil, expr)

  def Apply(fn: Tree, arg: Tree): Apply =
    Apply(fn, arg :: Nil)

  def ensureApplied(tpt: Tree) = tpt match {
    case _: Apply => tpt
    case _ => Apply(tpt, Nil)
  }

  def AppliedTypeTree(tpt: Tree, arg: Tree): AppliedTypeTree =
    AppliedTypeTree(tpt, arg :: Nil)

  def TypeTree(tpe: Type): TypedSplice = TypedSplice(TypeTree().withTypeUnchecked(tpe))

  def TypeDef(name: TypeName, tparams: List[TypeDef], rhs: Tree): TypeDef =
    if (tparams.isEmpty) TypeDef(name, rhs) else new PolyTypeDef(name, tparams, rhs)

  def unitLiteral = Literal(Constant(()))

  def ref(tp: NamedType)(implicit ctx: Context): Tree =
    TypedSplice(tpd.ref(tp))

  def rootDot(name: Name) = Select(Ident(nme.ROOTPKG), name)
  def scalaDot(name: Name) = Select(rootDot(nme.scala_), name)
  def scalaUnit = scalaDot(tpnme.Unit)

  def makeConstructor(tparams: List[TypeDef], vparamss: List[List[ValDef]], rhs: Tree = EmptyTree)(implicit ctx: Context): DefDef =
    DefDef(nme.CONSTRUCTOR, tparams, vparamss, TypeTree(), rhs)

  def emptyConstructor(implicit ctx: Context): DefDef =
    makeConstructor(Nil, Nil)

  def makeSelfDef(name: TermName, tpt: Tree)(implicit ctx: Context) =
    ValDef(name, tpt, EmptyTree).withFlags(PrivateLocal)

  def makeTupleOrParens(ts: List[Tree])(implicit ctx: Context) = ts match {
    case t :: Nil => Parens(t)
    case _ => Tuple(ts)
  }

  def makeTuple(ts: List[Tree])(implicit ctx: Context) = ts match {
    case t :: Nil => t
    case _ => Tuple(ts)
  }

  def makeParameter(pname: TermName, tpe: Tree, mods: Modifiers = EmptyModifiers)(implicit ctx: Context): ValDef =
    ValDef(pname, tpe, EmptyTree).withMods(mods | Param)

  def makeSyntheticParameter(n: Int = 1, tpt: Tree = TypeTree())(implicit ctx: Context): ValDef =
    ValDef(nme.syntheticParamName(n), tpt, EmptyTree).withFlags(SyntheticTermParam)

  /** A reference to given definition. If definition is a repeated
   *  parameter, the reference will be a repeated argument.
   */
  def refOfDef(tree: MemberDef)(implicit ctx: Context) = tree match {
    case ValDef(_, PostfixOp(_, nme.raw.STAR), _) => repeated(Ident(tree.name))
    case _ => Ident(tree.name)
  }

  /** A repeated argument such as `arg: _*` */
  def repeated(arg: Tree)(implicit ctx: Context) = Typed(arg, Ident(tpnme.WILDCARD_STAR))

// ------- Decorators -------------------------------------------------

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

  implicit class modsDeco(val mdef: MemberDef)(implicit ctx: Context) extends ModsDeco {
    def mods = mdef.rawMods
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

    def ModuleDef(tree: Tree)(name: TermName, impl: Template) = tree match {
      case tree: ModuleDef if (name eq tree.name) && (impl eq tree.impl) => tree
      case _ => untpd.ModuleDef(name, impl).withPos(tree.pos)
    }
    def PolyTypeDef(tree: Tree)(name: TypeName, tparams: List[TypeDef], rhs: Tree) = tree match {
      case tree: PolyTypeDef if (name eq tree.name) && (tparams eq tree.tparams) && (rhs eq tree.rhs) => tree
      case _ => new PolyTypeDef(name, tparams, rhs).withPos(tree.pos)
    }
    def SymbolLit(tree: Tree)(str: String) = tree match {
      case tree: SymbolLit if str == tree.str => tree
      case _ => untpd.SymbolLit(str).withPos(tree.pos)
    }
    def InterpolatedString(tree: Tree)(id: TermName, strings: List[Literal], elems: List[Tree]) = tree match {
      case tree: InterpolatedString if (id eq tree.id) && (strings eq tree.strings) && (elems eq tree.elems) => tree
      case _ => untpd.InterpolatedString(id, strings, elems).withPos(tree.pos)
    }
    def Function(tree: Tree)(args: List[Tree], body: Tree) = tree match {
      case tree: Function if (args eq tree.args) && (body eq tree.body) => tree
      case _ => untpd.Function(args, body).withPos(tree.pos)
    }
    def InfixOp(tree: Tree)(left: Tree, op: Name, right: Tree) = tree match {
      case tree: InfixOp if (left eq tree.left) && (op eq tree.op) && (right eq tree.right) => tree
      case _ => untpd.InfixOp(left, op, right).withPos(tree.pos)
    }
    def PostfixOp(tree: Tree)(od: Tree, op: Name) = tree match {
      case tree: PostfixOp if (od eq tree.od) && (op eq tree.op) => tree
      case _ => untpd.PostfixOp(od, op).withPos(tree.pos)
    }
    def PrefixOp(tree: Tree)(op: Name, od: Tree) = tree match {
      case tree: PrefixOp if (op eq tree.op) && (od eq tree.od) => tree
      case _ => untpd.PrefixOp(op, od).withPos(tree.pos)
    }
    def Parens(tree: Tree)(t: Tree) = tree match {
      case tree: Parens if t eq tree.t => tree
      case _ => untpd.Parens(t).withPos(tree.pos)
    }
    def Tuple(tree: Tree)(trees: List[Tree]) = tree match {
      case tree: Tuple if trees eq tree.trees => tree
      case _ => untpd.Tuple(trees).withPos(tree.pos)
    }
    def Throw(tree: Tree)(expr: Tree) = tree match {
      case tree: Throw if expr eq tree.expr => tree
      case _ => untpd.Throw(expr).withPos(tree.pos)
    }
    def WhileDo(tree: Tree)(cond: Tree, body: Tree) = tree match {
      case tree: WhileDo if (cond eq tree.cond) && (body eq tree.body) => tree
      case _ => untpd.WhileDo(cond, body).withPos(tree.pos)
    }
    def DoWhile(tree: Tree)(body: Tree, cond: Tree) = tree match {
      case tree: DoWhile if (body eq tree.body) && (cond eq tree.cond) => tree
      case _ => untpd.DoWhile(body, cond).withPos(tree.pos)
    }
    def ForYield(tree: Tree)(enums: List[Tree], expr: Tree) = tree match {
      case tree: ForYield if (enums eq tree.enums) && (expr eq tree.expr) => tree
      case _ => untpd.ForYield(enums, expr).withPos(tree.pos)
    }
    def ForDo(tree: Tree)(enums: List[Tree], body: Tree) = tree match {
      case tree: ForDo if (enums eq tree.enums) && (body eq tree.body) => tree
      case _ => untpd.ForDo(enums, body).withPos(tree.pos)
    }
    def GenFrom(tree: Tree)(pat: Tree, expr: Tree) = tree match {
      case tree: GenFrom if (pat eq tree.pat) && (expr eq tree.expr) => tree
      case _ => untpd.GenFrom(pat, expr).withPos(tree.pos)
    }
    def GenAlias(tree: Tree)(pat: Tree, expr: Tree) = tree match {
      case tree: GenAlias if (pat eq tree.pat) && (expr eq tree.expr) => tree
      case _ => untpd.GenAlias(pat, expr).withPos(tree.pos)
    }
    def ContextBounds(tree: Tree)(bounds: TypeBoundsTree, cxBounds: List[Tree]) = tree match {
      case tree: ContextBounds if (bounds eq tree.bounds) && (cxBounds eq tree.cxBounds) => tree
      case _ => untpd.ContextBounds(bounds, cxBounds).withPos(tree.pos)
    }
    def PatDef(tree: Tree)(mods: Modifiers, pats: List[Tree], tpt: Tree, rhs: Tree) = tree match {
      case tree: PatDef if (mods eq tree.mods) && (pats eq tree.pats) && (tpt eq tree.tpt) && (rhs eq tree.rhs) => tree
      case _ => untpd.PatDef(mods, pats, tpt, rhs).withPos(tree.pos)
    }
  }

  abstract class UntypedTreeMap(cpy: UntypedTreeCopier = untpd.cpy) extends TreeMap(cpy) {
    override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
      case ModuleDef(name, impl) =>
        cpy.ModuleDef(tree)(name, transformSub(impl))
      case SymbolLit(str) =>
        cpy.SymbolLit(tree)(str)
      case InterpolatedString(id, strings, elems) =>
        cpy.InterpolatedString(tree)(id, transformSub(strings), transform(elems))
      case Function(args, body) =>
        cpy.Function(tree)(transform(args), transform(body))
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
      case WhileDo(cond, body) =>
        cpy.WhileDo(tree)(transform(cond), transform(body))
      case DoWhile(body, cond) =>
        cpy.DoWhile(tree)(transform(body), transform(cond))
      case ForYield(enums, expr) =>
        cpy.ForYield(tree)(transform(enums), transform(expr))
      case ForDo(enums, body) =>
        cpy.ForDo(tree)(transform(enums), transform(body))
      case GenFrom(pat, expr) =>
        cpy.GenFrom(tree)(transform(pat), transform(expr))
      case GenAlias(pat, expr) =>
        cpy.GenAlias(tree)(transform(pat), transform(expr))
      case ContextBounds(bounds, cxBounds) =>
        cpy.ContextBounds(tree)(transformSub(bounds), transform(cxBounds))
      case PatDef(mods, pats, tpt, rhs) =>
        cpy.PatDef(tree)(mods, transform(pats), transform(tpt), transform(rhs))
      case tree: PolyTypeDef =>
        cpy.PolyTypeDef(tree)(tree.name, transformSub(tree.tparams), transform(tree.rhs))
      case _ =>
        super.transform(tree)
    }
  }

  abstract class UntypedTreeAccumulator[X] extends TreeAccumulator[X] {
    override def foldOver(x: X, tree: Tree)(implicit ctx: Context): X = tree match {
      case ModuleDef(name, impl) =>
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
      case Throw(expr) =>
        this(x, expr)
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
      case tree: PolyTypeDef =>
        this(this(x, tree.tparams), tree.rhs)
      case TypedSplice(tree) =>
        this(x, tree)
      case _ =>
        super.foldOver(x, tree)
    }
  }

  override def rename(tree: NameTree, newName: Name)(implicit ctx: Context): tree.ThisTree[Untyped] = tree match {
    case t: PolyTypeDef =>
      cpy.PolyTypeDef(t)(newName.asTypeName, t.tparams, t.rhs).asInstanceOf[tree.ThisTree[Untyped]]
    case _ => super.rename(tree, newName)
  }
}
