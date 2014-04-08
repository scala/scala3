package dotty.tools
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._
import CheckTrees._, Denotations._, Decorators._
import config.Printers._
import typer.ErrorReporting._

/** Some creators for typed trees */
object tpd extends Trees.Instance[Type] with TypedTreeInfo {

  private def ta(implicit ctx: Context) = ctx.typeAssigner

  def Modifiers(sym: Symbol)(implicit ctx: Context): Modifiers = Modifiers(
    sym.flags & ModifierFlags,
    if (sym.privateWithin.exists) sym.privateWithin.asType.name else tpnme.EMPTY,
    sym.annotations map (_.tree))

  def Ident(tp: NamedType)(implicit ctx: Context): Ident =
    ta.assignType(untpd.Ident(tp.name), tp)

  def Select(qualifier: Tree, name: Name)(implicit ctx: Context): Select =
    ta.assignType(untpd.Select(qualifier, name), qualifier)

  def Select(qualifier: Tree, tp: NamedType)(implicit ctx: Context): Select =
    untpd.Select(qualifier, tp.name).withType(tp)

  def Select(qualifier: Tree, sym: Symbol)(implicit ctx: Context): Select =
    untpd.Select(qualifier, sym.name).withType(
      TermRef.withSig(qualifier.tpe, sym.name.asTermName, sym.signature, sym.denot))

  def SelectWithSig(qualifier: Tree, name: Name, sig: Signature)(implicit ctx: Context) =
    untpd.SelectWithSig(qualifier, name, sig)
      .withType(TermRef.withSig(qualifier.tpe, name.asTermName, sig))

  def SelectFromTypeTree(qualifier: Tree, name: Name)(implicit ctx: Context): SelectFromTypeTree =
    ta.assignType(untpd.SelectFromTypeTree(qualifier, name), qualifier)

  def SelectFromTypeTree(qualifier: Tree, tp: NamedType)(implicit ctx: Context): SelectFromTypeTree =
    untpd.SelectFromTypeTree(qualifier, tp.name).withType(tp)

  def This(cls: ClassSymbol)(implicit ctx: Context): This =
    ta.assignType(untpd.This(cls.name))

  def Super(qual: Tree, mix: TypeName, inConstrCall: Boolean)(implicit ctx: Context): Super =
    ta.assignType(untpd.Super(qual, mix), qual, inConstrCall)

  def Apply(fn: Tree, args: List[Tree])(implicit ctx: Context): Apply =
    ta.assignType(untpd.Apply(fn, args), fn, args)

  def ensureApplied(fn: Tree)(implicit ctx: Context): Tree =
    if (fn.tpe.widen.isParameterless) fn else Apply(fn, Nil)

  def TypeApply(fn: Tree, args: List[Tree])(implicit ctx: Context): TypeApply =
    ta.assignType(untpd.TypeApply(fn, args), fn, args)

  def Literal(const: Constant)(implicit ctx: Context): Literal =
    ta.assignType(untpd.Literal(const))

  def unitLiteral(implicit ctx: Context): Literal =
    Literal(Constant(()))

  def New(tpt: Tree)(implicit ctx: Context): New =
    ta.assignType(untpd.New(tpt), tpt)

  def New(tp: Type)(implicit ctx: Context): New = New(TypeTree(tp))

  def Pair(left: Tree, right: Tree)(implicit ctx: Context): Pair =
    ta.assignType(untpd.Pair(left, right), left, right)

  def Typed(expr: Tree, tpt: Tree)(implicit ctx: Context): Typed =
    ta.assignType(untpd.Typed(expr, tpt), tpt)

  def NamedArg(name: Name, arg: Tree)(implicit ctx: Context) =
    ta.assignType(untpd.NamedArg(name, arg), arg)

  def Assign(lhs: Tree, rhs: Tree)(implicit ctx: Context): Assign =
    ta.assignType(untpd.Assign(lhs, rhs))

  def Block(stats: List[Tree], expr: Tree)(implicit ctx: Context): Block =
    ta.assignType(untpd.Block(stats, expr), stats, expr)

  def maybeBlock(stats: List[Tree], expr: Tree)(implicit ctx: Context): Tree =
    if (stats.isEmpty) expr else Block(stats, expr)

  def If(cond: Tree, thenp: Tree, elsep: Tree)(implicit ctx: Context): If =
    ta.assignType(untpd.If(cond, thenp, elsep), thenp, elsep)

  def Closure(env: List[Tree], meth: Tree, tpt: Tree)(implicit ctx: Context): Closure =
    ta.assignType(untpd.Closure(env, meth, tpt), meth, tpt)

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
  def Closure(meth: TermSymbol, rhsFn: List[List[Tree]] => Tree, targetType: Type = NoType)(implicit ctx: Context): Block = {
    val targetTpt = if (targetType.exists) TypeTree(targetType) else EmptyTree
    Block(
      DefDef(meth, rhsFn) :: Nil,
      Closure(Nil, Ident(TermRef(NoPrefix, meth)), targetTpt))
  }

  def CaseDef(pat: Tree, guard: Tree, body: Tree)(implicit ctx: Context): CaseDef =
    ta.assignType(untpd.CaseDef(pat, guard, body), body)

  def Match(selector: Tree, cases: List[CaseDef])(implicit ctx: Context): Match =
    ta.assignType(untpd.Match(selector, cases), cases)

  def Return(expr: Tree, from: Tree)(implicit ctx: Context): Return =
    ta.assignType(untpd.Return(expr, from))

  def Try(block: Tree, handler: Tree, finalizer: Tree)(implicit ctx: Context): Try =
    ta.assignType(untpd.Try(block, handler, finalizer), block, handler)

  def Throw(expr: Tree)(implicit ctx: Context): Throw =
    ta.assignType(untpd.Throw(expr))

  def SeqLiteral(elems: List[Tree])(implicit ctx: Context): SeqLiteral =
    ta.assignType(untpd.SeqLiteral(elems), elems)

  def SeqLiteral(tpe: Type, elems: List[Tree])(implicit ctx: Context): SeqLiteral =
    if (tpe derivesFrom defn.SeqClass) SeqLiteral(elems) else JavaSeqLiteral(elems)

  def JavaSeqLiteral(elems: List[Tree])(implicit ctx: Context): SeqLiteral =
    new untpd.JavaSeqLiteral(elems)
      .withType(defn.ArrayClass.typeRef.appliedTo(ctx.typeComparer.lub(elems.tpes)))

  def TypeTree(original: Tree)(implicit ctx: Context): TypeTree =
    TypeTree(original.tpe, original)

  def TypeTree(tp: Type, original: Tree = EmptyTree)(implicit ctx: Context): TypeTree =
    untpd.TypeTree(original).withType(tp).checked

  def SingletonTypeTree(ref: Tree)(implicit ctx: Context): SingletonTypeTree =
    ta.assignType(untpd.SingletonTypeTree(ref), ref)

  def AndTypeTree(left: Tree, right: Tree)(implicit ctx: Context): AndTypeTree =
    ta.assignType(untpd.AndTypeTree(left, right), left, right)

  def OrTypeTree(left: Tree, right: Tree)(implicit ctx: Context): OrTypeTree =
    ta.assignType(untpd.OrTypeTree(left, right), left, right)

  // RefinedTypeTree is missing, handled specially in Typer and Unpickler.

  def AppliedTypeTree(tycon: Tree, args: List[Tree])(implicit ctx: Context): AppliedTypeTree =
    ta.assignType(untpd.AppliedTypeTree(tycon, args), tycon, args)

  def ByNameTypeTree(result: Tree)(implicit ctx: Context): ByNameTypeTree =
    ta.assignType(untpd.ByNameTypeTree(result), result)

  def TypeBoundsTree(lo: Tree, hi: Tree)(implicit ctx: Context): TypeBoundsTree =
    ta.assignType(untpd.TypeBoundsTree(lo, hi), lo, hi)

  def Bind(sym: TermSymbol, body: Tree)(implicit ctx: Context): Bind =
    ta.assignType(untpd.Bind(sym.name, body), sym)

  def Alternative(trees: List[Tree])(implicit ctx: Context): Alternative =
    ta.assignType(untpd.Alternative(trees), trees)

  def UnApply(fun: Tree, implicits: List[Tree], patterns: List[Tree], proto: Type)(implicit ctx: Context): UnApply =
    ta.assignType(untpd.UnApply(fun, implicits, patterns), proto)

  def ValDef(sym: TermSymbol, rhs: Tree = EmptyTree)(implicit ctx: Context): ValDef =
    ta.assignType(untpd.ValDef(Modifiers(sym), sym.name, TypeTree(sym.info), rhs), sym)

  def SyntheticValDef(name: TermName, rhs: Tree)(implicit ctx: Context): ValDef =
    ValDef(ctx.newSymbol(ctx.owner, name, Synthetic, rhs.tpe, coord = rhs.pos), rhs)

  def DefDef(sym: TermSymbol, rhs: Tree = EmptyTree)(implicit ctx: Context): DefDef =
    ta.assignType(DefDef(sym, Function.const(rhs) _), sym)

  def DefDef(sym: TermSymbol, rhsFn: List[List[Tree]] => Tree)(implicit ctx: Context): DefDef = {
    val (tparams, mtp) = sym.info match {
      case tp: PolyType =>
        val tparams = ctx.newTypeParams(sym, tp.paramNames, EmptyFlags, tp.instantiateBounds)
        (tparams, tp.instantiate(tparams map (_.typeRef)))
      case tp => (Nil, tp)
    }

    def valueParamss(tp: Type): (List[List[TermSymbol]], Type) = tp match {
      case tp @ MethodType(paramNames, paramTypes) =>
        def valueParam(name: TermName, info: Type): TermSymbol =
          ctx.newSymbol(sym, name, TermParam, info)
        val params = (paramNames, paramTypes).zipped.map(valueParam)
        val (paramss, rtp) = valueParamss(tp.instantiate(params map (_.termRef)))
        (params :: paramss, rtp)
      case tp => (Nil, tp)
    }
    val (vparamss, rtp) = valueParamss(mtp)
    val argss = vparamss map (_ map (vparam => Ident(vparam.termRef)))
    ta.assignType(
      untpd.DefDef(
        Modifiers(sym), sym.name, tparams map TypeDef,
        vparamss map (_ map (ValDef(_))), TypeTree(rtp), rhsFn(argss)), sym)
  }

  def TypeDef(sym: TypeSymbol)(implicit ctx: Context): TypeDef =
    ta.assignType(untpd.TypeDef(Modifiers(sym), sym.name, TypeTree(sym.info)), sym)

  def ClassDef(cls: ClassSymbol, constr: DefDef, body: List[Tree])(implicit ctx: Context): TypeDef = {
    val parents = cls.info.parents map (TypeTree(_))
    val selfType =
      if (cls.classInfo.selfInfo ne NoType) ValDef(ctx.newSelfSym(cls))
      else EmptyValDef
    def isOwnTypeParam(stat: Tree) =
      (stat.symbol is TypeParam) && stat.symbol.owner == cls
    val bodyTypeParams = body filter isOwnTypeParam map (_.symbol)
    val newTypeParams =
      for (tparam <- cls.typeParams if !(bodyTypeParams contains tparam))
      yield TypeDef(tparam)
    val findLocalDummy = new FindLocalDummyAccumulator(cls)
    val localDummy = ((NoSymbol: Symbol) /: body)(findLocalDummy)
      .orElse(ctx.newLocalDummy(cls))
    val impl = untpd.Template(constr, parents, selfType, newTypeParams ++ body)
      .withType(localDummy.termRef).checked
    ta.assignType(untpd.TypeDef(Modifiers(cls), cls.name, impl), cls)
  }

  def Import(expr: Tree, selectors: List[untpd.Tree])(implicit ctx: Context): Import =
    ta.assignType(untpd.Import(expr, selectors), ctx.newImportSymbol(expr))

  def PackageDef(pid: RefTree, stats: List[Tree])(implicit ctx: Context): PackageDef =
    ta.assignType(untpd.PackageDef(pid, stats), pid)

  def Annotated(annot: Tree, arg: Tree)(implicit ctx: Context): Annotated =
    ta.assignType(untpd.Annotated(annot, arg), annot, arg)

  // ------ Making references ------------------------------------------------------

  /** A tree representing the same reference as the given type */
  def ref(tp: NamedType)(implicit ctx: Context): NameTree =
    if (tp.symbol.isStatic) Ident(tp)
    else tp.prefix match {
      case pre: TermRef => Select(ref(pre), tp)
      case pre => SelectFromTypeTree(TypeTree(pre), tp)
    } // no checks necessary

  def ref(sym: Symbol)(implicit ctx: Context): NameTree =
    ref(NamedType(sym.owner.thisType, sym.name, sym.denot))

  def singleton(tp: Type)(implicit ctx: Context): Tree = tp match {
    case tp: TermRef => ref(tp)
    case ThisType(cls) => This(cls)
    case SuperType(qual, _) => singleton(qual)
    case ConstantType(value) => Literal(value)
  }

  // ------ Creating typed equivalents of trees that exist only in untyped form -------

  /** new C(args) */
  def New(tp: Type, args: List[Tree])(implicit ctx: Context): Apply = {
    val targs = tp.argTypes
    val constr = tp.typeSymbol.primaryConstructor.asTerm
    Apply(
      Select(
        New(tp withoutArgs targs),
        TermRef.withSig(tp.normalizedPrefix, constr))
        .appliedToTypes(targs),
      args)
  }

  /** An object def
   *
   *     object obs extends parents { decls }
   *
   *  gets expanded to
   *
   *     <module> val obj = new obj$
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
    val constrSym = modcls.primaryConstructor orElse ctx.newDefaultConstructor(modcls).entered
    val constr = DefDef(constrSym.asTerm, EmptyTree)
    val clsdef = ClassDef(modcls, constr, body)
    val valdef = ValDef(sym, New(modcls.typeRef))
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
      copied.withTypeUnchecked(tree.tpe)
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

    def find[T](pred: (tpd.Tree) => Boolean): Option[tpd.Tree] =
      shallowFold[Option[tpd.Tree]](None)((accum, tree) => if (pred(tree)) Some(tree) else accum)

    def subst(from: List[Symbol], to: List[Symbol])(implicit ctx: Context): ThisTree =
      new TreeTypeMap(typeMap = new ctx.SubstSymMap(from, to)).apply(tree)

    def changeOwner(from: Symbol, to: Symbol)(implicit ctx: Context): ThisTree =
      new TreeTypeMap(ownerMap = (sym => if (sym == from) to else sym)).apply(tree)

    def appliedToTypes(targs: List[Type])(implicit ctx: Context): Tree =
      if (targs.isEmpty) tree else TypeApply(tree, targs map (TypeTree(_)))
  }

  implicit class ListOfTreeDecorator(val xs: List[tpd.Tree]) extends AnyVal {
    def tpes: List[Type] = xs map (_.tpe)
  }

  class TreeTypeMap(val typeMap: TypeMap = IdentityTypeMap, val ownerMap: Symbol => Symbol = identity _)(implicit ctx: Context) extends TreeMap {
    override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = super.transform {
      tree.withType(typeMap(tree.tpe)) match {
        case bind: tpd.Bind =>
          val sym = bind.symbol
          val newOwner = ownerMap(sym.owner)
          val newInfo = typeMap(sym.info)
          if ((newOwner ne sym.owner) || (newInfo ne sym.info))
            bind.withType(sym.copy(owner = newOwner, info = newInfo).namedType)
          else
            bind
        case tree1 =>
          tree1
      }
    }
    override def transformStats(trees: List[tpd.Tree])(implicit ctx: Context) = {
      val locals = ta.localSyms(trees)
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
      new TreeTypeMap(
        typeMap andThen ((tp: Type) => tp.substSym(from, to)),
        ownerMap andThen (from zip to).toMap)
  }

  // convert a numeric with a toXXX method
  def primitiveConversion(tree: Tree, numericCls: Symbol)(implicit ctx: Context): Tree = {
    val mname      = ("to" + numericCls.name).toTermName
    val conversion = tree.tpe member mname
    if (conversion.symbol.exists)
      ensureApplied(Select(tree, conversion.symbol.termRef))
    else if (tree.tpe.widen isRef numericCls)
      tree
    else {
      ctx.warning(i"conversion from ${tree.tpe.widen} to ${numericCls.typeRef} will always fail at runtime.")
      Throw(New(defn.ClassCastExceptionClass.typeRef, Nil)) withPos tree.pos
    }
  }

  def evalOnce(tree: Tree)(within: Tree => Tree)(implicit ctx: Context) = {
    if (isIdempotentExpr(tree)) within(tree)
    else {
      val vdef = SyntheticValDef(ctx.freshName("ev$").toTermName, tree)
      Block(vdef :: Nil, within(Ident(vdef.namedType)))
    }
  }

  def runtimeCall(name: TermName, args: List[Tree])(implicit ctx: Context): Tree = ???

  def mkAnd(tree1: Tree, tree2: Tree)(implicit ctx: Context) =
    Apply(Select(tree1, defn.Boolean_and), tree2 :: Nil)

  // ensure that constructors are fully applied?
  // ensure that normal methods are fully applied?

}

