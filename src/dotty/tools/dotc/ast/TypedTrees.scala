package dotty.tools
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._
import CheckTrees._, Denotations._

object tpd extends Trees.Instance[Type] with TypedTreeInfo {

  def Modifiers(sym: Symbol)(implicit ctx: Context): Modifiers = Modifiers(
    sym.flags & ModifierFlags,
    if (sym.privateWithin.exists) sym.privateWithin.asType.name else tpnme.EMPTY,
    sym.annotations map (_.tree))

  override def Ident(name: Name)(implicit ctx: Context): Ident = unsupported("Ident")
  override def BackquotedIdent(name: Name)(implicit ctx: Context): BackquotedIdent = unsupported("BackquotedIdent")

  def Ident(tp: NamedType)(implicit ctx: Context): Ident =
    super.Ident(tp.name).withType(tp.underlyingIfRepeated).checked

  override def Select(qualifier: Tree, name: Name)(implicit ctx: Context): Select =
    Select(qualifier, NamedType(qualifier.tpe, name))

  def Select(pre: Tree, tp: NamedType)(implicit ctx: Context): Select =
    super.Select(pre, tp.name).withType(tp).checked

  override def SelectWithSig(qualifier: Tree, name: Name, sig: Signature)(implicit ctx: Context) =
    super.SelectWithSig(qualifier, name, sig)
      .withType(TermRef.withSig(qualifier.tpe, name.asTermName, sig))

  override def This(qual: TypeName)(implicit ctx: Context): This = unsupported("This")

  def This(cls: ClassSymbol)(implicit ctx: Context): This =
    super.This(cls.name).withType(cls.thisType).checked

  override def Super(qual: Tree, mix: TypeName)(implicit ctx: Context): Super = {
    val owntype =
      if (mix.isEmpty) ctx.glb(qual.tpe.parents)
      else {
        val mixParents = qual.tpe.parents filter (_.name == mix)
        check(mixParents.length == 1)
        mixParents.head
      }
    super.Super(qual, mix).withType(SuperType(qual.tpe, owntype)).checked
  }

  override def Apply(fn: Tree, args: List[Tree])(implicit ctx: Context): Apply = {
    val owntype = fn.tpe.widen match {
      case fntpe @ MethodType(pnames, ptypes) =>
        check(sameLength(ptypes, args), s"${fn.show}: ${fntpe.show} to ${args.map(_.show).mkString(", ")}")
        fntpe.instantiate(args map (_.tpe))
      case _ =>
        check(false)
        ErrorType
    }
    super.Apply(fn, args).withType(owntype).checked
  }

  override def TypeApply(fn: Tree, args: List[Tree])(implicit ctx: Context): TypeApply = {
    val owntype = fn.tpe.widen match {
      case fntpe @ PolyType(pnames) =>
        check(sameLength(pnames, args))
        fntpe.instantiate(args map (_.tpe))
      case _ =>
        check(false)
        ErrorType
    }
    super.TypeApply(fn, args).withType(owntype).checked
  }

  override def Literal(const: Constant)(implicit ctx: Context): Literal =
    super.Literal(const).withType(const.tpe).checked

  override def New(tpt: Tree)(implicit ctx: Context): New =
    super.New(tpt).withType(tpt.tpe).checked

  def New(tp: Type)(implicit ctx: Context): New = New(TypeTree(tp))

  override def Pair(left: Tree, right: Tree)(implicit ctx: Context): Pair =
    super.Pair(left, right).withType(defn.PairType.appliedTo(left.tpe, right.tpe)).checked

  override def Typed(expr: Tree, tpt: Tree)(implicit ctx: Context): Typed =
    super.Typed(expr, tpt).withType(tpt.tpe).checked

  override def NamedArg(name: Name, arg: Tree)(implicit ctx: Context) =
    super.NamedArg(name, arg).withType(arg.tpe).checked

  override def Assign(lhs: Tree, rhs: Tree)(implicit ctx: Context): Assign =
    super.Assign(lhs, rhs).withType(defn.UnitType).checked

  override def Block(stats: List[Tree], expr: Tree)(implicit ctx: Context): Block = {
    lazy val locals = localSyms(stats).toSet
    val blk = super.Block(stats, expr)
    def widen(tp: Type): Type = tp match {
      case tp: TermRef if locals contains tp.symbol =>
        widen(tp.info)
      case _ => tp
    }
    blk.withType(widen(expr.tpe))
  }

  def maybeBlock(stats: List[Tree], expr: Tree)(implicit ctx: Context): Tree =
    if (stats.isEmpty) expr else Block(stats, expr)

  override def If(cond: Tree, thenp: Tree, elsep: Tree)(implicit ctx: Context): If =
    super.If(cond, thenp, elsep).withType(thenp.tpe | elsep.tpe).checked

  override def Closure(env: List[Tree], meth: RefTree)(implicit ctx: Context): Closure = {
    val ownType = meth.tpe.widen match {
      case mt @ MethodType(_, formals) =>
        assert(!mt.isDependent)
        val formals1 = formals mapConserve (_.underlyingIfRepeated)
        defn.FunctionType(formals1, mt.resultType)
    }
    super.Closure(env, meth).withType(ownType).checked
  }

  /** A function def
   *
   *    vparams => expr
   *
   *  gets expanded to
   *
   *    { def $anonfun(vparams) = expr; Closure($anonfun) }
   *
   *  where the closure's type is the target type of the expression (FunctionN, unless
   *  otherwise specified).
   */
  def Closure(meth: TermSymbol, bodyFn: List[Tree] => Tree)(implicit ctx: Context): Block = {
    val rhsFn: List[List[Tree]] => Tree = { case args :: Nil => bodyFn(args) }
    Block(
      DefDef(meth, rhsFn) :: Nil,
      Closure(Nil, Ident(TermRef.withSym(NoPrefix, meth))))
  }

  override def Match(selector: Tree, cases: List[CaseDef])(implicit ctx: Context): Match =
    super.Match(selector, cases).withType(ctx.lub(cases map (_.body.tpe))).checked

  override def CaseDef(pat: Tree, guard: Tree, body: Tree)(implicit ctx: Context): CaseDef =
    super.CaseDef(pat, guard, body).withType(body.tpe).checked

  override def Return(expr: Tree, from: Tree)(implicit ctx: Context): Return =
    super.Return(expr, from).withType(defn.NothingType).checked

  override def Try(block: Tree, handler: Tree, finalizer: Tree)(implicit ctx: Context): Try =
    super.Try(block, handler, finalizer).withType(block.tpe | handler.tpe).checked

  override def Throw(expr: Tree)(implicit ctx: Context): Throw =
    super.Throw(expr).withType(defn.NothingType).checked

  override def SeqLiteral(elems: List[Tree])(implicit ctx: Context): SeqLiteral =
    SeqLiteral(defn.SeqClass.typeConstructor.appliedTo(
        ctx.lub(elems map (_.tpe)) :: Nil), elems)

  // TODO: Split into Java/Scala eq literals
  def SeqLiteral(tpe: Type, elems: List[Tree])(implicit ctx: Context): SeqLiteral =
    super.SeqLiteral(elems).withType(tpe).checked

  override def TypeTree(): TypeTree = unsupported("TypeTree()")

  override def TypeTree(original: Tree)(implicit ctx: Context): TypeTree =
    TypeTree(original.tpe, original)

  def TypeTree(tp: Type, original: Tree = EmptyTree)(implicit ctx: Context): TypeTree =
    super.TypeTree(original).withType(tp).checked

  override def SingletonTypeTree(ref: Tree)(implicit ctx: Context): SingletonTypeTree =
    super.SingletonTypeTree(ref).withType(ref.tpe).checked

  override def SelectFromTypeTree(qualifier: Tree, name: Name)(implicit ctx: Context): SelectFromTypeTree =
    SelectFromTypeTree(qualifier, NamedType(qualifier.tpe, name))

  def SelectFromTypeTree(qualifier: Tree, tp: NamedType)(implicit ctx: Context): SelectFromTypeTree =
    super.SelectFromTypeTree(qualifier, tp.name).withType(tp).checked

  override def AndTypeTree(left: Tree, right: Tree)(implicit ctx: Context): AndTypeTree =
    super.AndTypeTree(left, right).withType(left.tpe & right.tpe).checked

  override def OrTypeTree(left: Tree, right: Tree)(implicit ctx: Context): OrTypeTree =
    super.OrTypeTree(left, right).withType(left.tpe | right.tpe).checked

  override def RefinedTypeTree(tpt: Tree, refinements: List[Tree])(implicit ctx: Context): RefinedTypeTree = {
    def refineType(tp: Type, refinement: Symbol): Type =
      RefinedType(tp, refinement.name, refinement.info)
    super.RefinedTypeTree(tpt, refinements)
      .withType((tpt.tpe /: (refinements map (_.symbol)))(refineType)).checked
  }

  def refineType(tp: Type, refinement: Symbol)(implicit ctx: Context): Type =
    RefinedType(tp, refinement.name, refinement.info)

  override def AppliedTypeTree(tpt: Tree, args: List[Tree])(implicit ctx: Context): AppliedTypeTree =
    super.AppliedTypeTree(tpt, args).withType(tpt.tpe.appliedTo(args map (_.tpe))).checked

  override def TypeBoundsTree(lo: Tree, hi: Tree)(implicit ctx: Context): TypeBoundsTree =
    super.TypeBoundsTree(lo, hi).withType(TypeBounds(lo.tpe, hi.tpe)).checked

  override def Bind(name: Name, body: Tree)(implicit ctx: Context): Bind = unsupported("Bind")

  def Bind(sym: TermSymbol, body: Tree)(implicit ctx: Context): Bind =
    super.Bind(sym.name, body).withType(refType(sym)).checked

  override def Alternative(trees: List[Tree])(implicit ctx: Context): Alternative =
    super.Alternative(trees).withType(ctx.lub(trees map (_.tpe))).checked

  override def UnApply(fun: Tree, args: List[Tree])(implicit ctx: Context): UnApply = {
    val owntype = fun.tpe.widen match {
      case MethodType(_, paramType :: Nil) => paramType
      case _ => check(false); ErrorType
    }
    super.UnApply(fun, args).withType(owntype).checked
  }

  override def ValDef(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree)(implicit ctx: Context): ValDef = unsupported("ValDef")

  def ValDef(sym: TermSymbol, rhs: Tree = EmptyTree)(implicit ctx: Context): ValDef =
    super.ValDef(Modifiers(sym), sym.name, TypeTree(sym.info), rhs).withType(refType(sym)).checked

  override def DefDef(mods: Modifiers, name: TermName, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree)(implicit ctx: Context): DefDef = unsupported("DefDef")

  def DefDef(sym: TermSymbol, rhs: Tree = EmptyTree)(implicit ctx: Context): DefDef =
    DefDef(sym, Function.const(rhs) _)

  def DefDef(sym: TermSymbol, rhsFn: List[List[Tree]] => Tree)(implicit ctx: Context): DefDef = {

    val (tparams, mtp) = sym.info match {
      case tp: PolyType =>
        val tparams = ctx.newTypeParams(sym, tp.paramNames, EmptyFlags, tp.instantiateBounds)
        (tparams, tp.instantiate(tparams map (_.typeConstructor)))
      case tp => (Nil, tp)
    }

    def valueParamss(tp: Type): (List[List[TermSymbol]], Type) = tp match {
      case tp @ MethodType(paramNames, paramTypes) =>
        def valueParam(name: TermName, info: Type): TermSymbol =
          ctx.newSymbol(sym, name, TermParam, info)
        val params = (paramNames, paramTypes).zipped.map(valueParam)
        val (paramss, rtp) = valueParamss(tp.instantiate(params map (_.typeConstructor)))
        (params :: paramss, rtp)
      case tp => (Nil, tp)
    }
    val (vparamss, rtp) = valueParamss(mtp)
    val argss = vparamss map (_ map (vparam => Ident(vparam.symRef)))
    super.DefDef(
      Modifiers(sym), sym.name, tparams map TypeDef,
      vparamss map (_ map (ValDef(_))), TypeTree(rtp), rhsFn(argss))
      .withType(refType(sym)).checked
  }

  override def TypeDef(mods: Modifiers, name: TypeName, rhs: Tree)(implicit ctx: Context): TypeDef = unsupported("TypeDef")

  def TypeDef(sym: TypeSymbol)(implicit ctx: Context): TypeDef =
    super.TypeDef(Modifiers(sym), sym.name, TypeTree(sym.info))
      .withType(refType(sym)).checked

  def ClassDef(cls: ClassSymbol, typeParams: List[TypeSymbol], constr: DefDef, body: List[Tree])(implicit ctx: Context): TypeDef = {
    val parents = cls.info.parents map (TypeTree(_))
    val selfType =
      if (cls.classInfo.optSelfType.exists) ValDef(ctx.newSelfSym(cls))
      else EmptyValDef
    def isOwnTypeParamAccessor(stat: Tree) =
      (stat.symbol is TypeParam) && stat.symbol.owner == cls
    val (tparamAccessors, rest) = body partition isOwnTypeParamAccessor
    val tparams =
      (typeParams map TypeDef) ++
        (tparamAccessors collect {
          case td: TypeDef if !(typeParams contains td.symbol) => td
        })
    val findLocalDummy = new FindLocalDummyAccumulator(cls)
    val localDummy = ((NoSymbol: Symbol) /: body)(findLocalDummy)
      .orElse(ctx.newLocalDummy(cls))
    val impl = super.Template(constr, parents, selfType, rest)
      .withType(refType(localDummy)).checked
    super.TypeDef(Modifiers(cls), cls.name, impl) // !!! todo: revise
      .withType(refType(cls)).checked
  }

  override def Import(expr: Tree, selectors: List[untpd.Tree])(implicit ctx: Context): Import =
    super.Import(expr, selectors).withType(refType(ctx.newImportSymbol(SharedTree(expr)))).checked

  override def PackageDef(pid: RefTree, stats: List[Tree])(implicit ctx: Context): PackageDef =
    super.PackageDef(pid, stats).withType(refType(pid.symbol)).checked

  override def Annotated(annot: Tree, arg: Tree)(implicit ctx: Context): Annotated =
    super.Annotated(annot, arg).withType(AnnotatedType(Annotation(annot), arg.tpe)).checked

  override def SharedTree(tree: Tree)(implicit ctx: Context): SharedTree =
    Trees.SharedTree(tree).withType(tree.tpe)


  // ------ Making references ------------------------------------------------------

  def refType(sym: Symbol)(implicit ctx: Context): NamedType = NamedType.withSym(sym.owner.thisType, sym)

  /** A tree representing the same reference as the given type */
  def ref(tp: NamedType)(implicit ctx: Context): NameTree =
    if (tp.symbol.isStatic) Ident(tp)
    else tp.prefix match {
      case pre: TermRef => Select(ref(pre), tp)
      case pre => SelectFromTypeTree(TypeTree(pre), tp)
    } // no checks necessary

  def ref(sym: Symbol)(implicit ctx: Context): tpd.NameTree =
    ref(NamedType(sym.owner.thisType, sym.name).withDenot(sym))

  // ------ Creating typed equivalents of trees that exist only in untyped form -------

  /** new C(args) */
  def New(tp: Type, args: List[Tree])(implicit ctx: Context): Apply =
    Apply(
      Select(
        New(tp),
        TermRef.withSym(tp.normalizedPrefix, tp.typeSymbol.primaryConstructor.asTerm)),
      args)

  /** An object def
   *
   *     object obs extends parents { decls }
   *
   *  gets expanded to
   *
   *     <module> lazy val obj = new obj$
   *     <module> class obj$ extends parents { this: obj.type => decls }
   *
   *  (The following no longer applies:
   *  What's interesting here is that the block is well typed
   *  (because class obj$ is hoistable), but the type of the `obj` val is
   *  not expressible. What needs to happen in general when
   *  inferring the type of a val from its RHS, is: if the type contains
   *  a class that has the val itself as owner, then that class
   *  is remapped to have the val's owner as owner. Remapping could be
   *  done by cloning the class with the new owner and substituting
   *  everywhere in the tree. We know that remapping is safe
   *  because the only way a local class can appear in the RHS of a val is
   *  by being hoisted outside of a block, and the necessary checks are
   *  done at this point already.
   *
   *  On the other hand, for method result type inference, if the type of
   *  the RHS of a method contains a class owned by the method, this would be
   *  an error.)
   */
  def ModuleDef(sym: TermSymbol, body: List[Tree])(implicit ctx: Context): tpd.Thicket = {
    val modcls = sym.moduleClass.asClass
    val constr = DefDef(modcls.primaryConstructor.asTerm, EmptyTree)
    val clsdef = ClassDef(modcls, Nil, constr, body)
    val valdef = ValDef(sym, New(modcls.typeConstructor))
    Thicket(valdef, clsdef)
  }

  private class FindLocalDummyAccumulator(cls: ClassSymbol)(implicit ctx: Context) extends TreeAccumulator[Symbol] {
    def apply(sym: Symbol, tree: Tree) =
      if (sym.exists) sym
      else if (tree.isDef) {
        val owner = tree.symbol.owner
        if (owner.isLocalDummy && owner.owner == cls) owner
        else if (owner == cls) foldOver(sym, tree)
        else sym
      } else foldOver(sym, tree)
  }

  override val cpy = new TypedTreeCopier

  class TypedTreeCopier extends TreeCopier {
    def postProcess(tree: Tree, copied: untpd.Tree): copied.ThisTree[Type] =
      copied.withType(tree.tpe)
  }

  implicit class TreeOps[ThisTree <: tpd.Tree](val tree: ThisTree) extends AnyVal {

    def isValue(implicit ctx: Context): Boolean =
      tree.isTerm && tree.tpe.widen.isValueType

    def isValueOrPattern(implicit ctx: Context) =
      tree.isValue || tree.isPattern

    def isValueType: Boolean =
      tree.isType && tree.tpe.isValueType

    def isInstantiation: Boolean = tree match {
      case Apply(Select(New(_), nme.CONSTRUCTOR), _) => true
      case _ => false
    }

    def checked(implicit ctx: Context): ThisTree = {
      if (ctx.settings.YcheckTypedTrees.value) checkType(tree)
      tree
    }

    def shallowFold[T](z: T)(op: (T, tpd.Tree) => T) =
      new ShallowFolder(op).apply(z, tree)

    def deepFold[T](z: T)(op: (T, tpd.Tree) => T) =
      new DeepFolder(op).apply(z, tree)

    def subst(from: List[Symbol], to: List[Symbol])(implicit ctx: Context): ThisTree =
      new TreeMapper(typeMap = new ctx.SubstSymMap(from, to)).apply(tree)

    def changeOwner(from: Symbol, to: Symbol)(implicit ctx: Context): ThisTree =
      new TreeMapper(ownerMap = (sym => if (sym == from) to else sym)).apply(tree)
  }

  class TreeMapper(val typeMap: TypeMap = IdentityTypeMap, val ownerMap: Symbol => Symbol = identity)(implicit ctx: Context) extends TreeTransformer {
    override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = super.transform {
      tree.withType(typeMap(tree.tpe)) match {
        case bind: tpd.Bind =>
          val sym = bind.symbol
          val newOwner = ownerMap(sym.owner)
          val newInfo = typeMap(sym.info)
          if ((newOwner ne sym.owner) || (newInfo ne sym.info))
            bind.withType(tpd.refType(sym.copy(owner = newOwner, info = newInfo)))
          else
            bind
        case tree1 =>
          tree1
      }
    }
    override def transformStats(trees: List[tpd.Tree])(implicit ctx: Context) = {
      val locals = localSyms(trees)
      val mapped = ctx.mapSymbols(locals, typeMap, ownerMap)
      if (locals eq mapped) super.transform(trees)
      else withSubstitution(locals, mapped).transform(trees)
    }

    def apply[ThisTree <: tpd.Tree](tree: ThisTree): ThisTree = transform(tree).asInstanceOf[ThisTree]

    def apply(annot: Annotation): Annotation = {
      val tree1 = apply(annot.tree)
      if (tree1 eq annot.tree) annot else ConcreteAnnotation(tree1)
    }

    /** The current tree map composed with a substitution [from -> to] */
    def withSubstitution(from: List[Symbol], to: List[Symbol]) =
      new TreeMapper(
        typeMap andThen ((tp: Type) => tp.substSym(from, to)),
        ownerMap andThen (from zip to).toMap)
  }

  // ensure that constructors are fully applied?
  // ensure that normal methods are fully applied?

  def localSyms(stats: List[tpd.Tree])(implicit ctx: Context): List[Symbol] =
    for (stat <- stats if (stat.isDef)) yield stat.symbol
}

