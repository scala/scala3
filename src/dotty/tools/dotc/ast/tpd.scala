package dotty.tools
package dotc
package ast

import dotty.tools.dotc.transform.{ExplicitOuter, Erasure}
import dotty.tools.dotc.typer.ProtoTypes.FunProtoTyped
import transform.SymUtils._
import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, Symbols._
import Denotations._, Decorators._, DenotTransformers._
import config.Printers._
import collection.mutable
import typer.ErrorReporting._

import scala.annotation.tailrec

/** Some creators for typed trees */
object tpd extends Trees.Instance[Type] with TypedTreeInfo {

  private def ta(implicit ctx: Context) = ctx.typeAssigner

  def Modifiers(sym: Symbol)(implicit ctx: Context): Modifiers = Modifiers(
    sym.flags & (if (sym.isType) ModifierFlags | VarianceFlags else ModifierFlags),
    if (sym.privateWithin.exists) sym.privateWithin.asType.name else tpnme.EMPTY,
    sym.annotations map (_.tree))

  def Ident(tp: NamedType)(implicit ctx: Context): Ident =
    ta.assignType(untpd.Ident(tp.name), tp)

  def Select(qualifier: Tree, name: Name)(implicit ctx: Context): Select =
    ta.assignType(untpd.Select(qualifier, name), qualifier)

  def SelectFromTypeTree(qualifier: Tree, name: Name)(implicit ctx: Context): SelectFromTypeTree =
    ta.assignType(untpd.SelectFromTypeTree(qualifier, name), qualifier)

  def SelectFromTypeTree(qualifier: Tree, tp: NamedType)(implicit ctx: Context): SelectFromTypeTree =
    untpd.SelectFromTypeTree(qualifier, tp.name).withType(tp)

  def This(cls: ClassSymbol)(implicit ctx: Context): This =
    untpd.This(cls.name).withType(cls.thisType)

  def Super(qual: Tree, mix: TypeName, inConstrCall: Boolean, mixinClass: Symbol = NoSymbol)(implicit ctx: Context): Super =
    ta.assignType(untpd.Super(qual, mix), qual, inConstrCall, mixinClass)

  def Apply(fn: Tree, args: List[Tree])(implicit ctx: Context): Apply =
    ta.assignType(untpd.Apply(fn, args), fn, args)

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

  def NamedArg(name: Name, arg: Tree)(implicit ctx: Context): NamedArg =
    ta.assignType(untpd.NamedArg(name, arg), arg)

  def Assign(lhs: Tree, rhs: Tree)(implicit ctx: Context): Assign =
    ta.assignType(untpd.Assign(lhs, rhs))

  def Block(stats: List[Tree], expr: Tree)(implicit ctx: Context): Block =
    ta.assignType(untpd.Block(stats, expr), stats, expr)

  /** Join `stats` in front of `expr` creating a new block if necessary */
  def seq(stats: List[Tree], expr: Tree)(implicit ctx: Context): Tree =
    if (stats.isEmpty) expr
    else expr match {
      case Block(estats, eexpr) => cpy.Block(expr)(stats ::: estats, eexpr)
      case _ => Block(stats, expr)
    }

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
  def Closure(meth: TermSymbol, rhsFn: List[List[Tree]] => Tree, targs: List[Tree] = Nil, targetType: Type = NoType)(implicit ctx: Context): Block = {
    val targetTpt = if (targetType.exists) TypeTree(targetType) else EmptyTree
    val call =
      if (targs.isEmpty) Ident(TermRef(NoPrefix, meth))
      else TypeApply(Ident(TermRef(NoPrefix, meth)), targs)
    Block(
      DefDef(meth, rhsFn) :: Nil,
      Closure(Nil, call, targetTpt))
  }

  def CaseDef(pat: Tree, guard: Tree, body: Tree)(implicit ctx: Context): CaseDef =
    ta.assignType(untpd.CaseDef(pat, guard, body), body)

  def Match(selector: Tree, cases: List[CaseDef])(implicit ctx: Context): Match =
    ta.assignType(untpd.Match(selector, cases), cases)

  def Return(expr: Tree, from: Tree)(implicit ctx: Context): Return =
    ta.assignType(untpd.Return(expr, from))

  def Try(block: Tree, cases: List[CaseDef], finalizer: Tree)(implicit ctx: Context): Try =
    ta.assignType(untpd.Try(block, cases, finalizer), block, cases)

  def SeqLiteral(elems: List[Tree], elemtpt: Tree)(implicit ctx: Context): SeqLiteral =
    ta.assignType(untpd.SeqLiteral(elems, elemtpt), elems, elemtpt)

  def JavaSeqLiteral(elems: List[Tree], elemtpt: Tree)(implicit ctx: Context): JavaSeqLiteral =
    ta.assignType(new untpd.JavaSeqLiteral(elems, elemtpt), elems, elemtpt).asInstanceOf[JavaSeqLiteral]

  def TypeTree(original: Tree)(implicit ctx: Context): TypeTree =
    TypeTree(original.tpe, original)

  def TypeTree(tp: Type, original: Tree = EmptyTree)(implicit ctx: Context): TypeTree =
    untpd.TypeTree(original).withType(tp)

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

  /** A pattern corresponding to `sym: tpe` */
  def BindTyped(sym: TermSymbol, tpe: Type)(implicit ctx: Context): Bind =
    Bind(sym, Typed(Underscore(tpe), TypeTree(tpe)))

  def Alternative(trees: List[Tree])(implicit ctx: Context): Alternative =
    ta.assignType(untpd.Alternative(trees), trees)

  def UnApply(fun: Tree, implicits: List[Tree], patterns: List[Tree], proto: Type)(implicit ctx: Context): UnApply =
    ta.assignType(untpd.UnApply(fun, implicits, patterns), proto)

  def ValDef(sym: TermSymbol, rhs: LazyTree = EmptyTree)(implicit ctx: Context): ValDef =
    ta.assignType(untpd.ValDef(sym.name, TypeTree(sym.info), rhs), sym)

  def SyntheticValDef(name: TermName, rhs: Tree)(implicit ctx: Context): ValDef =
    ValDef(ctx.newSymbol(ctx.owner, name, Synthetic, rhs.tpe.widen, coord = rhs.pos), rhs)

  def DefDef(sym: TermSymbol, rhs: Tree = EmptyTree)(implicit ctx: Context): DefDef =
    ta.assignType(DefDef(sym, Function.const(rhs) _), sym)

  def DefDef(sym: TermSymbol, rhsFn: List[List[Tree]] => Tree)(implicit ctx: Context): DefDef =
    polyDefDef(sym, Function.const(rhsFn))

  def polyDefDef(sym: TermSymbol, rhsFn: List[Type] => List[List[Tree]] => Tree)(implicit ctx: Context): DefDef = {
    val (tparams, mtp) = sym.info match {
      case tp: PolyType =>
        val tparams = ctx.newTypeParams(sym, tp.paramNames, EmptyFlags, tp.instantiateBounds)
        (tparams, tp.instantiate(tparams map (_.typeRef)))
      case tp => (Nil, tp)
    }

    def valueParamss(tp: Type): (List[List[TermSymbol]], Type) = tp match {
      case tp @ MethodType(paramNames, paramTypes) =>
        def valueParam(name: TermName, info: Type): TermSymbol = {
          val maybeImplicit = if (tp.isInstanceOf[ImplicitMethodType]) Implicit else EmptyFlags
          ctx.newSymbol(sym, name, TermParam | maybeImplicit, info)
        }
        val params = (paramNames, paramTypes).zipped.map(valueParam)
        val (paramss, rtp) = valueParamss(tp.instantiate(params map (_.termRef)))
        (params :: paramss, rtp)
      case tp => (Nil, tp.widenExpr)
    }
    val (vparamss, rtp) = valueParamss(mtp)
    val targs = tparams map (_.typeRef)
    val argss = vparamss.nestedMap(vparam => Ident(vparam.termRef))
    ta.assignType(
      untpd.DefDef(
        sym.name,
        tparams map TypeDef,
        vparamss.nestedMap(ValDef(_)),
        TypeTree(rtp),
        rhsFn(targs)(argss)),
      sym)
  }

  def TypeDef(sym: TypeSymbol)(implicit ctx: Context): TypeDef =
    ta.assignType(untpd.TypeDef(sym.name, TypeTree(sym.info)), sym)

  def ClassDef(cls: ClassSymbol, constr: DefDef, body: List[Tree], superArgs: List[Tree] = Nil)(implicit ctx: Context): TypeDef = {
    val firstParentRef :: otherParentRefs = cls.info.parents
    val firstParent = cls.typeRef.baseTypeWithArgs(firstParentRef.symbol)
    val superRef =
      if (cls is Trait) TypeTree(firstParent)
      else {
        def isApplicable(ctpe: Type): Boolean = ctpe match {
          case ctpe: PolyType =>
            isApplicable(ctpe.instantiate(firstParent.argTypes))
          case ctpe: MethodType =>
            (superArgs corresponds ctpe.paramTypes)(_.tpe <:< _)
          case _ =>
            false
        }
        val constr = firstParent.decl(nme.CONSTRUCTOR).suchThat(constr => isApplicable(constr.info))
        New(firstParent, constr.symbol.asTerm, superArgs)
      }
    val parents = superRef :: otherParentRefs.map(TypeTree(_))

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
    val localDummy = ((NoSymbol: Symbol) /: body)(findLocalDummy.apply)
      .orElse(ctx.newLocalDummy(cls))
    val impl = untpd.Template(constr, parents, selfType, newTypeParams ++ body)
      .withType(localDummy.nonMemberTermRef)
    ta.assignType(untpd.TypeDef(cls.name, impl), cls)
  }

  /** An anonymous class
   *
   *      new parents { forwarders }
   *
   *  where `forwarders` contains forwarders for all functions in `fns`.
   *  @param parents    a non-empty list of class types
   *  @param fns        a non-empty of functions for which forwarders should be defined in the class.
   *  The class has the same owner as the first function in `fns`.
   *  Its position is the union of all functions in `fns`.
   */
  def AnonClass(parents: List[Type], fns: List[TermSymbol], methNames: List[TermName])(implicit ctx: Context): Block = {
    val owner = fns.head.owner
    val parents1 =
      if (parents.head.classSymbol.is(Trait)) defn.ObjectType :: parents
      else parents
    val cls = ctx.newNormalizedClassSymbol(owner, tpnme.ANON_FUN, Synthetic, parents1,
        coord = fns.map(_.pos).reduceLeft(_ union _))
    val constr = ctx.newConstructor(cls, Synthetic, Nil, Nil).entered
    def forwarder(fn: TermSymbol, name: TermName) = {
      val fwdMeth = fn.copy(cls, name, Synthetic | Method).entered.asTerm
      DefDef(fwdMeth, prefss => ref(fn).appliedToArgss(prefss))
    }
    val forwarders = (fns, methNames).zipped.map(forwarder)
    val cdef = ClassDef(cls, DefDef(constr), forwarders)
    Block(cdef :: Nil, New(cls.typeRef, Nil))
  }

  // { <label> def while$(): Unit = if (cond) { body; while$() } ; while$() }
  def WhileDo(owner: Symbol, cond: Tree, body: List[Tree])(implicit ctx: Context): Tree = {
    val sym = ctx.newSymbol(owner, nme.WHILE_PREFIX, Flags.Label | Flags.Synthetic,
      MethodType(Nil, defn.UnitType), coord = cond.pos)

    val call = Apply(ref(sym), Nil)
    val rhs = If(cond, Block(body, call), unitLiteral)
    Block(List(DefDef(sym, rhs)), call)
  }

  def Import(expr: Tree, selectors: List[untpd.Tree])(implicit ctx: Context): Import =
    ta.assignType(untpd.Import(expr, selectors), ctx.newImportSymbol(ctx.owner, expr))

  def PackageDef(pid: RefTree, stats: List[Tree])(implicit ctx: Context): PackageDef =
    ta.assignType(untpd.PackageDef(pid, stats), pid)

  def Annotated(annot: Tree, arg: Tree)(implicit ctx: Context): Annotated =
    ta.assignType(untpd.Annotated(annot, arg), annot, arg)

  def Throw(expr: Tree)(implicit ctx: Context): Tree =
    ref(defn.throwMethod).appliedTo(expr)

  // ------ Making references ------------------------------------------------------

  def prefixIsElidable(tp: NamedType)(implicit ctx: Context) = {
    def test(implicit ctx: Context) = tp.prefix match {
      case NoPrefix =>
        true
      case pre: ThisType =>
        pre.cls.isStaticOwner ||
          tp.symbol.is(ParamOrAccessor) && !pre.cls.is(Trait) && ctx.owner.enclosingClass == pre.cls
          // was ctx.owner.enclosingClass.derivesFrom(pre.cls) which was not tight enough
          // and was spuriously triggered in case inner class would inherit from outer one
          // eg anonymous TypeMap inside TypeMap.andThen
      case pre: TermRef =>
        pre.symbol.is(Module) && pre.symbol.isStatic
      case _ =>
        false
    }
    try test || tp.symbol.is(JavaStatic) || tp.symbol.hasAnnotation(defn.ScalaStaticAnnot)
    catch { // See remark in SymDenotations#accessWithin
      case ex: NotDefinedHere => test(ctx.addMode(Mode.FutureDefsOK))
    }
  }

  def needsSelect(tp: Type)(implicit ctx: Context) = tp match {
    case tp: TermRef => !prefixIsElidable(tp)
    case _ => false
  }

  /** A tree representing the same reference as the given type */
  def ref(tp: NamedType)(implicit ctx: Context): Tree =
    if (tp.isType) TypeTree(tp)
    else if (prefixIsElidable(tp)) Ident(tp)
    else if (tp.symbol.is(Module) && ctx.owner.isContainedIn(tp.symbol.moduleClass))
      followOuterLinks(This(tp.symbol.moduleClass.asClass))
    else if (tp.symbol hasAnnotation defn.ScalaStaticAnnot)
      Ident(tp)
    else tp.prefix match {
      case pre: SingletonType => followOuterLinks(singleton(pre)).select(tp)
      case pre => SelectFromTypeTree(TypeTree(pre), tp)
    } // no checks necessary

  def ref(sym: Symbol)(implicit ctx: Context): Tree =
    ref(NamedType(sym.owner.thisType, sym.name, sym.denot))

  private def followOuterLinks(t: Tree)(implicit ctx: Context) = t match {
    case t: This if ctx.erasedTypes && !(t.symbol == ctx.owner.enclosingClass || t.symbol.isStaticOwner) =>
      // after erasure outer paths should be respected
      new ExplicitOuter.OuterOps(ctx).path(t.tpe.widen.classSymbol)
    case t =>
      t
  }

  def singleton(tp: Type)(implicit ctx: Context): Tree = tp match {
    case tp: TermRef => ref(tp)
    case tp: ThisType => This(tp.cls)
    case SuperType(qual, _) => singleton(qual)
    case ConstantType(value) => Literal(value)
  }

  /** A tree representing a `newXYZArray` operation of the right
   *  kind for the given element type in `typeArg`. No type arguments or
   *  `length` arguments are given.
   */
  def newArray(elemTpe: Type, returnTpe: Type, pos: Position, dims: JavaSeqLiteral)(implicit ctx: Context): Tree = {
    val elemClass = elemTpe.classSymbol
    def newArr =
      ref(defn.DottyArraysModule).select(defn.newArrayMethod).withPos(pos)

    if (!ctx.erasedTypes) {
      assert(!TypeErasure.isUnboundedGeneric(elemTpe)) //needs to be done during typer. See Applications.convertNewGenericArray
      newArr.appliedToTypeTrees(TypeTree(returnTpe) :: Nil).appliedToArgs(clsOf(elemTpe) :: clsOf(returnTpe) :: dims :: Nil).withPos(pos)
    } else  // after erasure
      newArr.appliedToArgs(clsOf(elemTpe) :: clsOf(returnTpe) :: dims :: Nil).withPos(pos)
  }

  // ------ Creating typed equivalents of trees that exist only in untyped form -------

  /** new C(args), calling the primary constructor of C */
  def New(tp: Type, args: List[Tree])(implicit ctx: Context): Apply =
    New(tp, tp.typeSymbol.primaryConstructor.asTerm, args)

  /** new C(args), calling given constructor `constr` of C */
  def New(tp: Type, constr: TermSymbol, args: List[Tree])(implicit ctx: Context): Apply = {
    val targs = tp.argTypes
    val tycon = tp.withoutArgs(targs)
    New(tycon)
      .select(TermRef.withSig(tycon, constr))
      .appliedToTypes(targs)
      .appliedToArgs(args)
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
    val valdef = ValDef(sym, New(modcls.typeRef).select(constrSym).appliedToNone)
    Thicket(valdef, clsdef)
  }

  /** A `_' with given type */
  def Underscore(tp: Type)(implicit ctx: Context) = untpd.Ident(nme.WILDCARD).withType(tp)

  def defaultValue(tpe: Types.Type)(implicit ctx: Context) = {
    val tpw = tpe.widen

    if (tpw isRef defn.IntClass) Literal(Constant(0))
    else if (tpw isRef defn.LongClass) Literal(Constant(0L))
    else if (tpw isRef defn.BooleanClass) Literal(Constant(false))
    else if (tpw isRef defn.CharClass) Literal(Constant('\u0000'))
    else if (tpw isRef defn.FloatClass) Literal(Constant(0f))
    else if (tpw isRef defn.DoubleClass) Literal(Constant(0d))
    else if (tpw isRef defn.ByteClass) Literal(Constant(0.toByte))
    else if (tpw isRef defn.ShortClass) Literal(Constant(0.toShort))
    else Literal(Constant(null)).select(defn.Any_asInstanceOf).appliedToType(tpe)
  }

  private class FindLocalDummyAccumulator(cls: ClassSymbol)(implicit ctx: Context) extends TreeAccumulator[Symbol] {
    def apply(sym: Symbol, tree: Tree)(implicit ctx: Context) =
      if (sym.exists) sym
      else if (tree.isDef) {
        val owner = tree.symbol.owner
        if (owner.isLocalDummy && owner.owner == cls) owner
        else if (owner == cls) foldOver(sym, tree)
        else sym
      } else foldOver(sym, tree)
  }

  implicit class modsDeco(mdef: MemberDef)(implicit ctx: Context) extends ModsDeco {
    def mods = if (mdef.hasType) Modifiers(mdef.symbol) else mdef.rawMods
  }

  override val cpy = new TypedTreeCopier

  class TypedTreeCopier extends TreeCopier {
    def postProcess(tree: Tree, copied: untpd.Tree): copied.ThisTree[Type] =
      copied.withTypeUnchecked(tree.tpe)
    def postProcess(tree: Tree, copied: untpd.MemberDef): copied.ThisTree[Type] =
      copied.withTypeUnchecked(tree.tpe)

    override def Select(tree: Tree)(qualifier: Tree, name: Name)(implicit ctx: Context): Select = {
      val tree1 = untpd.cpy.Select(tree)(qualifier, name)
      tree match {
        case tree: Select if qualifier.tpe eq tree.qualifier.tpe =>
          tree1.withTypeUnchecked(tree.tpe)
        case _ => tree.tpe match {
          case tpe: NamedType => tree1.withType(tpe.derivedSelect(qualifier.tpe))
          case _ => tree1.withTypeUnchecked(tree.tpe)
        }
      }
    }

    override def Apply(tree: Tree)(fun: Tree, args: List[Tree])(implicit ctx: Context): Apply =
      ta.assignType(untpd.cpy.Apply(tree)(fun, args), fun, args)
      // Note: Reassigning the original type if `fun` and `args` have the same types as before
      // does not work here: The computed type depends on the widened function type, not
      // the function type itself. A treetransform may keep the function type the
      // same but its widened type might change.

    override def TypeApply(tree: Tree)(fun: Tree, args: List[Tree])(implicit ctx: Context): TypeApply =
      ta.assignType(untpd.cpy.TypeApply(tree)(fun, args), fun, args)
      // Same remark as for Apply

    override def Literal(tree: Tree)(const: Constant)(implicit ctx: Context): Literal =
      ta.assignType(untpd.cpy.Literal(tree)(const))

    override def New(tree: Tree)(tpt: Tree)(implicit ctx: Context): New =
      ta.assignType(untpd.cpy.New(tree)(tpt), tpt)

    override def Pair(tree: Tree)(left: Tree, right: Tree)(implicit ctx: Context): Pair = {
      val tree1 = untpd.cpy.Pair(tree)(left, right)
      tree match {
        case tree: Pair if (left.tpe eq tree.left.tpe) && (right.tpe eq tree.right.tpe) => tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, left, right)
      }
    }

    override def Typed(tree: Tree)(expr: Tree, tpt: Tree)(implicit ctx: Context): Typed =
      ta.assignType(untpd.cpy.Typed(tree)(expr, tpt), tpt)

    override def NamedArg(tree: Tree)(name: Name, arg: Tree)(implicit ctx: Context): NamedArg =
      ta.assignType(untpd.cpy.NamedArg(tree)(name, arg), arg)

    override def Assign(tree: Tree)(lhs: Tree, rhs: Tree)(implicit ctx: Context): Assign =
      ta.assignType(untpd.cpy.Assign(tree)(lhs, rhs))

    override def Block(tree: Tree)(stats: List[Tree], expr: Tree)(implicit ctx: Context): Block = {
      val tree1 = untpd.cpy.Block(tree)(stats, expr)
      tree match {
        case tree: Block if expr.tpe eq tree.expr.tpe => tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, stats, expr)
      }
    }

    override def If(tree: Tree)(cond: Tree, thenp: Tree, elsep: Tree)(implicit ctx: Context): If = {
      val tree1 = untpd.cpy.If(tree)(cond, thenp, elsep)
      tree match {
        case tree: If if (thenp.tpe eq tree.thenp.tpe) && (elsep.tpe eq tree.elsep.tpe) => tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, thenp, elsep)
      }
    }

    override def Closure(tree: Tree)(env: List[Tree], meth: Tree, tpt: Tree)(implicit ctx: Context): Closure =
      ta.assignType(untpd.cpy.Closure(tree)(env, meth, tpt), meth, tpt)
      // Same remark as for Apply

    override def Match(tree: Tree)(selector: Tree, cases: List[CaseDef])(implicit ctx: Context): Match = {
      val tree1 = untpd.cpy.Match(tree)(selector, cases)
      tree match {
        case tree: Match if sameTypes(cases, tree.cases) => tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, cases)
      }
    }

    override def CaseDef(tree: Tree)(pat: Tree, guard: Tree, body: Tree)(implicit ctx: Context): CaseDef = {
      val tree1 = untpd.cpy.CaseDef(tree)(pat, guard, body)
      tree match {
        case tree: CaseDef if body.tpe eq tree.body.tpe => tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, body)
      }
    }

    override def Return(tree: Tree)(expr: Tree, from: Tree)(implicit ctx: Context): Return =
      ta.assignType(untpd.cpy.Return(tree)(expr, from))

    override def Try(tree: Tree)(expr: Tree, cases: List[CaseDef], finalizer: Tree)(implicit ctx: Context): Try = {
      val tree1 = untpd.cpy.Try(tree)(expr, cases, finalizer)
      tree match {
        case tree: Try if (expr.tpe eq tree.expr.tpe) && sameTypes(cases, tree.cases) => tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, expr, cases)
      }
    }

    override def SeqLiteral(tree: Tree)(elems: List[Tree], elemtpt: Tree)(implicit ctx: Context): SeqLiteral = {
      val tree1 = untpd.cpy.SeqLiteral(tree)(elems, elemtpt)
      tree match {
        case tree: SeqLiteral
        if sameTypes(elems, tree.elems) && (elemtpt.tpe eq tree.elemtpt.tpe) =>
          tree1.withTypeUnchecked(tree.tpe)
        case _ =>
          ta.assignType(tree1, elems, elemtpt)
      }
    }

    override def Annotated(tree: Tree)(annot: Tree, arg: Tree)(implicit ctx: Context): Annotated = {
      val tree1 = untpd.cpy.Annotated(tree)(annot, arg)
      tree match {
        case tree: Annotated if (arg.tpe eq tree.arg.tpe) && (annot eq tree.annot) => tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, annot, arg)
      }
    }

    override def If(tree: If)(cond: Tree = tree.cond, thenp: Tree = tree.thenp, elsep: Tree = tree.elsep)(implicit ctx: Context): If =
      If(tree: Tree)(cond, thenp, elsep)
    override def Closure(tree: Closure)(env: List[Tree] = tree.env, meth: Tree = tree.meth, tpt: Tree = tree.tpt)(implicit ctx: Context): Closure =
      Closure(tree: Tree)(env, meth, tpt)
    override def CaseDef(tree: CaseDef)(pat: Tree = tree.pat, guard: Tree = tree.guard, body: Tree = tree.body)(implicit ctx: Context): CaseDef =
      CaseDef(tree: Tree)(pat, guard, body)
    override def Try(tree: Try)(expr: Tree = tree.expr, cases: List[CaseDef] = tree.cases, finalizer: Tree = tree.finalizer)(implicit ctx: Context): Try =
      Try(tree: Tree)(expr, cases, finalizer)
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

    def shallowFold[T](z: T)(op: (T, tpd.Tree) => T)(implicit ctx: Context) =
      new ShallowFolder(op).apply(z, tree)

    def deepFold[T](z: T)(op: (T, tpd.Tree) => T)(implicit ctx: Context) =
      new DeepFolder(op).apply(z, tree)

    def find[T](pred: (tpd.Tree) => Boolean)(implicit ctx: Context): Option[tpd.Tree] =
      shallowFold[Option[tpd.Tree]](None)((accum, tree) => if (pred(tree)) Some(tree) else accum)

    def subst(from: List[Symbol], to: List[Symbol])(implicit ctx: Context): ThisTree =
      new TreeTypeMap(substFrom = from, substTo = to).apply(tree)

    /** Change owner from `from` to `to`. If `from` is a weak owner, also change its
     *  owner to `to`, and continue until a non-weak owner is reached.
     */
    def changeOwner(from: Symbol, to: Symbol)(implicit ctx: Context): ThisTree = {
      def loop(from: Symbol, froms: List[Symbol], tos: List[Symbol]): ThisTree = {
        if (from.isWeakOwner && !from.owner.isClass)
          loop(from.owner, from :: froms, to :: tos)
        else {
          //println(i"change owner ${from :: froms}%, % ==> $tos of $tree")
          new TreeTypeMap(oldOwners = from :: froms, newOwners = tos)(ctx.addMode(Mode.FutureDefsOK)).apply(tree)
        }
      }
      loop(from, Nil, to :: Nil)
    }

    /** After phase `trans`, set the owner of every definition in this tree that was formerly
     *  owner by `from` to `to`.
     */
    def changeOwnerAfter(from: Symbol, to: Symbol, trans: DenotTransformer)(implicit ctx: Context): ThisTree = {
      assert(ctx.phase == trans.next)
      val traverser = new TreeTraverser {
        def traverse(tree: Tree)(implicit ctx: Context) = tree match {
          case tree: DefTree =>
            val sym = tree.symbol
            val prevDenot = sym.denot(ctx.withPhase(trans))
            if (prevDenot.owner == from) {
              val d = sym.copySymDenotation(owner = to)
              d.installAfter(trans)
              d.transformAfter(trans, d => if (d.owner eq from) d.copySymDenotation(owner = to) else d)
            }
            if (sym.isWeakOwner) traverseChildren(tree)
          case _ =>
            traverseChildren(tree)
        }
      }
      traverser.traverse(tree)(ctx.addMode(Mode.FutureDefsOK))
      tree
    }

    /** A select node with the given selector name and a computed type */
    def select(name: Name)(implicit ctx: Context): Select =
      Select(tree, name)

    /** A select node with the given type */
    def select(tp: NamedType)(implicit ctx: Context): Select =
      untpd.Select(tree, tp.name).withType(tp)

    /** A select node that selects the given symbol. Note: Need to make sure this
     *  is in fact the symbol you would get when you select with the symbol's name,
     *  otherwise a data race may occur which would be flagged by -Yno-double-bindings.
     */
    def select(sym: Symbol)(implicit ctx: Context): Select = {
      val tp =
        if (sym.isType)
          TypeRef(tree.tpe, sym.name.asTypeName)
        else
          TermRef.withSigAndDenot(tree.tpe, sym.name.asTermName,
            sym.signature, sym.denot.asSeenFrom(tree.tpe))
      untpd.Select(tree, sym.name)
        .withType(tp)
    }

    /** A select node with the given selector name and signature and a computed type */
    def selectWithSig(name: Name, sig: Signature)(implicit ctx: Context): Tree =
      untpd.SelectWithSig(tree, name, sig)
        .withType(TermRef.withSig(tree.tpe, name.asTermName, sig))

    /** A select node with selector name and signature taken from `sym`.
     *  Note: Use this method instead of select(sym) if the referenced symbol
     *  might be overridden in the type of the qualifier prefix. See note
     *  on select(sym: Symbol).
     */
    def selectWithSig(sym: Symbol)(implicit ctx: Context): Tree =
      selectWithSig(sym.name, sym.signature)

    /** A unary apply node with given argument: `tree(arg)` */
    def appliedTo(arg: Tree)(implicit ctx: Context): Tree =
      appliedToArgs(arg :: Nil)

    /** An apply node with given arguments: `tree(arg, args0, ..., argsN)` */
    def appliedTo(arg: Tree, args: Tree*)(implicit ctx: Context): Tree =
      appliedToArgs(arg :: args.toList)

    /** An apply node with given argument list `tree(args(0), ..., args(args.length - 1))` */
    def appliedToArgs(args: List[Tree])(implicit ctx: Context): Apply =
      Apply(tree, args)

    /** The current tree applied to given argument lists:
     *  `tree (argss(0)) ... (argss(argss.length -1))`
     */
    def appliedToArgss(argss: List[List[Tree]])(implicit ctx: Context): Tree =
      ((tree: Tree) /: argss)(Apply(_, _))

    /** The current tree applied to (): `tree()` */
    def appliedToNone(implicit ctx: Context): Apply = appliedToArgs(Nil)

    /** The current tree applied to given type argument: `tree[targ]` */
    def appliedToType(targ: Type)(implicit ctx: Context): Tree =
      appliedToTypes(targ :: Nil)

    /** The current tree applied to given type arguments: `tree[targ0, ..., targN]` */
    def appliedToTypes(targs: List[Type])(implicit ctx: Context): Tree =
      appliedToTypeTrees(targs map (TypeTree(_)))

    /** The current tree applied to given type argument list: `tree[targs(0), ..., targs(targs.length - 1)]` */
    def appliedToTypeTrees(targs: List[Tree])(implicit ctx: Context): Tree =
      if (targs.isEmpty) tree else TypeApply(tree, targs)

    /** Apply to `()` unless tree's widened type is parameterless */
    def ensureApplied(implicit ctx: Context): Tree =
      if (tree.tpe.widen.isParameterless) tree else tree.appliedToNone

    /** `tree.isInstanceOf[tp]` */
    def isInstance(tp: Type)(implicit ctx: Context): Tree =
      tree.select(defn.Any_isInstanceOf).appliedToType(tp)

    /** tree.asInstanceOf[`tp`] */
    def asInstance(tp: Type)(implicit ctx: Context): Tree = {
      assert(tp.isValueType, i"bad cast: $tree.asInstanceOf[$tp]")
      tree.select(defn.Any_asInstanceOf).appliedToType(tp)
    }

    /** `tree.asInstanceOf[tp]` (or its box/unbox/cast equivalent when after
     *  erasure and value and non-value types are mixed),
     *  unless tree's type already conforms to `tp`.
     */
    def ensureConforms(tp: Type)(implicit ctx: Context): Tree =
      if (tree.tpe <:< tp) tree
      else if (!ctx.erasedTypes) asInstance(tp)
      else Erasure.Boxing.adaptToType(tree, tp)

    /** If inititializer tree is `_', the default value of its type,
     *  otherwise the tree itself.
     */
    def wildcardToDefault(implicit ctx: Context) =
      if (isWildcardArg(tree)) defaultValue(tree.tpe) else tree

    /** `this && that`, for boolean trees `this`, `that` */
    def and(that: Tree)(implicit ctx: Context): Tree =
      tree.select(defn.Boolean_&&).appliedTo(that)

    /** `this || that`, for boolean trees `this`, `that` */
    def or(that: Tree)(implicit ctx: Context): Tree =
      tree.select(defn.Boolean_||).appliedTo(that)

    /** The translation of `tree = rhs`.
     *  This is either the tree as an assignment, to a setter call.
     */
    def becomes(rhs: Tree)(implicit ctx: Context): Tree =
      if (tree.symbol is Method) {
        val setr = tree match {
          case Ident(_) =>
            val setter = tree.symbol.setter
            assert(setter.exists, tree.symbol.showLocated)
            ref(tree.symbol.setter)
          case Select(qual, _) => qual.select(tree.symbol.setter)
        }
        setr.appliedTo(rhs)
      }
      else Assign(tree, rhs)

    // --- Higher order traversal methods -------------------------------

    /** Apply `f` to each subtree of this tree */
    def foreachSubTree(f: Tree => Unit)(implicit ctx: Context): Unit = {
      val traverser = new TreeTraverser {
        def traverse(tree: Tree)(implicit ctx: Context) = foldOver(f(tree), tree)
      }
      traverser.traverse(tree)
    }

    /** Is there a subtree of this tree that satisfies predicate `p`? */
    def existsSubTree(p: Tree => Boolean)(implicit ctx: Context): Boolean = {
      val acc = new TreeAccumulator[Boolean] {
        def apply(x: Boolean, t: Tree)(implicit ctx: Context) = x || p(t) || foldOver(x, t)
      }
      acc(false, tree)
    }

    /** All subtrees of this tree that satisfy predicate `p`. */
    def filterSubTrees(f: Tree => Boolean)(implicit ctx: Context): List[Tree] = {
      val buf = new mutable.ListBuffer[Tree]
      foreachSubTree { tree => if (f(tree)) buf += tree }
      buf.toList
    }
  }

  implicit class ListOfTreeDecorator(val xs: List[tpd.Tree]) extends AnyVal {
    def tpes: List[Type] = xs map (_.tpe)
  }

  // convert a numeric with a toXXX method
  def primitiveConversion(tree: Tree, numericCls: Symbol)(implicit ctx: Context): Tree = {
    val mname      = ("to" + numericCls.name).toTermName
    val conversion = tree.tpe member mname
    if (conversion.symbol.exists)
      tree.select(conversion.symbol.termRef).ensureApplied
    else if (tree.tpe.widen isRef numericCls)
      tree
    else {
      ctx.warning(i"conversion from ${tree.tpe.widen} to ${numericCls.typeRef} will always fail at runtime.")
      Throw(New(defn.ClassCastExceptionClass.typeRef, Nil)) withPos tree.pos
    }
  }

  /** A tree that represents the class of the erasure of type `tp`. */
  def clsOf(tp: Type)(implicit ctx: Context): Tree = {
    def TYPE(module: TermSymbol) = ref(module).select(nme.TYPE_)
    defn.scalaClassName(tp) match {
      case tpnme.Boolean => TYPE(defn.BoxedBooleanModule)
      case tpnme.Byte => TYPE(defn.BoxedByteModule)
      case tpnme.Short => TYPE(defn.BoxedShortModule)
      case tpnme.Char => TYPE(defn.BoxedCharModule)
      case tpnme.Int => TYPE(defn.BoxedIntModule)
      case tpnme.Long => TYPE(defn.BoxedLongModule)
      case tpnme.Float => TYPE(defn.BoxedFloatModule)
      case tpnme.Double => TYPE(defn.BoxedDoubleModule)
      case tpnme.Unit => TYPE(defn.BoxedUnitModule)
      case _ =>
        if(ctx.erasedTypes || !tp.derivesFrom(defn.ArrayClass))
          Literal(Constant(TypeErasure.erasure(tp)))
        else Literal(Constant(tp))
    }
  }

  def applyOverloaded(receiver: Tree, method: TermName, args: List[Tree], targs: List[Type], expectedType: Type, isAnnotConstructor: Boolean = false)(implicit ctx: Context): Tree = {
    val typer = ctx.typer
    val proto = new FunProtoTyped(args, expectedType, typer)
    val denot = receiver.tpe.member(method)
    assert(denot.exists, i"no member $receiver . $method, members = ${receiver.tpe.decls}")
    val selected =
      if (denot.isOverloaded) {
        def typeParamCount(tp: Type) = tp.widen match {
          case tp: PolyType => tp.paramBounds.length
          case _ => 0
        }
        var allAlts = denot.alternatives
          .map(_.termRef).filter(tr => typeParamCount(tr) == targs.length)
        if (targs.isEmpty) allAlts = allAlts.filterNot(_.widen.isInstanceOf[PolyType])
        val alternatives =
          ctx.typer.resolveOverloaded(allAlts, proto, Nil)
        assert(alternatives.size == 1,
          i"${if (alternatives.isEmpty) "no" else "multiple"} overloads available for " +
          i"$method on ${receiver.tpe.widenDealias} with targs: $targs%, %; args: $args%, % of types ${args.tpes}%, %; expectedType: $expectedType." +
          i" isAnnotConstructor = $isAnnotConstructor.\n" +
          i"all alternatives: ${allAlts.map(_.symbol.showDcl).mkString(", ")}\n" +
          i"matching alternatives: ${alternatives.map(_.symbol.showDcl).mkString(", ")}.") // this is parsed from bytecode tree. there's nothing user can do about it
        alternatives.head
      }
      else denot.asSingleDenotation.termRef
    val fun = receiver
      .select(TermRef.withSig(receiver.tpe, selected.termSymbol.asTerm))
      .appliedToTypes(targs)

    def adaptLastArg(lastParam: Tree, expectedType: Type) = {
      if (isAnnotConstructor && !(lastParam.tpe <:< expectedType)) {
        val defn = ctx.definitions
        val prefix = args.take(selected.widen.paramTypess.head.size - 1)
        expectedType match {
          case defn.ArrayOf(el) =>
            lastParam.tpe match {
              case defn.ArrayOf(el2) if el2 <:< el =>
                // we have a JavaSeqLiteral with a more precise type
                // we cannot construct a tree as JavaSeqLiteral infered to precise type
                // if we add typed than it would be both type-correct and
                // will pass Ycheck
                prefix ::: List(tpd.Typed(lastParam, TypeTree(defn.ArrayOf(el))))
              case _ =>
                ???
            }
          case _ => args
        }
      } else args
    }

    val callArgs: List[Tree] = if (args.isEmpty) Nil else {
      val expectedType = selected.widen.paramTypess.head.last
      val lastParam = args.last
      adaptLastArg(lastParam, expectedType)
    }

    val apply = untpd.Apply(fun, callArgs)
    new typer.ApplyToTyped(apply, fun, selected, callArgs, expectedType).result.asInstanceOf[Tree] // needed to handle varargs
  }

  @tailrec
  def sameTypes(trees: List[tpd.Tree], trees1: List[tpd.Tree]): Boolean = {
    if (trees.isEmpty) trees.isEmpty
    else if (trees1.isEmpty) trees.isEmpty
    else (trees.head.tpe eq trees1.head.tpe) && sameTypes(trees.tail, trees1.tail)
  }

  def evalOnce(tree: Tree)(within: Tree => Tree)(implicit ctx: Context) = {
    if (isIdempotentExpr(tree)) within(tree)
    else {
      val vdef = SyntheticValDef(ctx.freshName("ev$").toTermName, tree)
      Block(vdef :: Nil, within(Ident(vdef.namedType)))
    }
  }

  def runtimeCall(name: TermName, args: List[Tree])(implicit ctx: Context): Tree = {
    Ident(defn.ScalaRuntimeModule.requiredMethod(name).termRef).appliedToArgs(args)
  }

  /** An extractor that pulls out type arguments */
  object MaybePoly {
    def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
      case TypeApply(tree, targs) => Some(tree, targs)
      case _ => Some(tree, Nil)
    }
  }

  /** A traverser that passes the enclosing class or method as an argument
   *  to the traverse method.
   */
  abstract class EnclosingMethodTraverser extends TreeAccumulator[Symbol] {
    def traverse(enclMeth: Symbol, tree: Tree)(implicit ctx: Context): Unit
    def apply(enclMeth: Symbol, tree: Tree)(implicit ctx: Context) = {
      tree match {
        case _: DefTree if tree.symbol.exists =>
          traverse(tree.symbol.enclosingMethod, tree)
        case _ =>
          traverse(enclMeth, tree)
      }
      enclMeth
    }
  }

  // ensure that constructors are fully applied?
  // ensure that normal methods are fully applied?

}

