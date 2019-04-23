package dotty.tools
package dotc
package ast

import dotty.tools.dotc.transform.{ExplicitOuter, Erasure}
import dotty.tools.dotc.typer.ProtoTypes.FunProtoTyped
import transform.SymUtils._
import transform.TypeUtils._
import core._
import util.Spans._, Types._, Contexts._, Constants._, Names._, Flags._, NameOps._
import Symbols._, StdNames._, Annotations._, Trees._, Symbols._
import Decorators._, DenotTransformers._
import collection.{immutable, mutable}
import util.{Property, SourceFile, NoSource}
import NameKinds.{TempResultName, OuterSelectName}
import typer.ConstFold

import scala.annotation.tailrec
import scala.io.Codec

/** Some creators for typed trees */
object tpd extends Trees.Instance[Type] with TypedTreeInfo {

  private def ta(implicit ctx: Context) = ctx.typeAssigner

  def Ident(tp: NamedType)(implicit ctx: Context): Ident =
    ta.assignType(untpd.Ident(tp.name), tp)

  def Select(qualifier: Tree, name: Name)(implicit ctx: Context): Select =
    ta.assignType(untpd.Select(qualifier, name), qualifier)

  def Select(qualifier: Tree, tp: NamedType)(implicit ctx: Context): Select =
    untpd.Select(qualifier, tp.name).withType(tp)

  def This(cls: ClassSymbol)(implicit ctx: Context): This =
    untpd.This(untpd.Ident(cls.name)).withType(cls.thisType)

  def Super(qual: Tree, mix: untpd.Ident, inConstrCall: Boolean, mixinClass: Symbol)(implicit ctx: Context): Super =
    ta.assignType(untpd.Super(qual, mix), qual, inConstrCall, mixinClass)

  def Super(qual: Tree, mixName: TypeName, inConstrCall: Boolean, mixinClass: Symbol = NoSymbol)(implicit ctx: Context): Super =
    Super(qual, if (mixName.isEmpty) untpd.EmptyTypeIdent else untpd.Ident(mixName), inConstrCall, mixinClass)

  def Apply(fn: Tree, args: List[Tree])(implicit ctx: Context): Apply = {
    assert(fn.isInstanceOf[RefTree] || fn.isInstanceOf[GenericApply[_]])
    ta.assignType(untpd.Apply(fn, args), fn, args)
  }

  def TypeApply(fn: Tree, args: List[Tree])(implicit ctx: Context): TypeApply = {
    assert(fn.isInstanceOf[RefTree] || fn.isInstanceOf[GenericApply[_]])
    ta.assignType(untpd.TypeApply(fn, args), fn, args)
  }

  def Literal(const: Constant)(implicit ctx: Context): Literal =
    ta.assignType(untpd.Literal(const))

  def unitLiteral(implicit ctx: Context): Literal =
    Literal(Constant(()))

  def New(tpt: Tree)(implicit ctx: Context): New =
    ta.assignType(untpd.New(tpt), tpt)

  def New(tp: Type)(implicit ctx: Context): New = New(TypeTree(tp))

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
      case Block(estats, eexpr) =>
        cpy.Block(expr)(stats ::: estats, eexpr).withType(ta.avoidingType(eexpr, stats))
      case _ => Block(stats, expr)
    }

  def If(cond: Tree, thenp: Tree, elsep: Tree)(implicit ctx: Context): If =
    ta.assignType(untpd.If(cond, thenp, elsep), thenp, elsep)

  def InlineIf(cond: Tree, thenp: Tree, elsep: Tree)(implicit ctx: Context): If =
    ta.assignType(untpd.InlineIf(cond, thenp, elsep), thenp, elsep)

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

  /** A closure whole anonymous function has the given method type */
  def Lambda(tpe: MethodType, rhsFn: List[Tree] => Tree)(implicit ctx: Context): Block = {
    val meth = ctx.newSymbol(ctx.owner, nme.ANON_FUN, Synthetic | Method, tpe)
    Closure(meth, tss => rhsFn(tss.head).changeOwner(ctx.owner, meth))
  }

  def CaseDef(pat: Tree, guard: Tree, body: Tree)(implicit ctx: Context): CaseDef =
    ta.assignType(untpd.CaseDef(pat, guard, body), pat, body)

  def Match(selector: Tree, cases: List[CaseDef])(implicit ctx: Context): Match =
    ta.assignType(untpd.Match(selector, cases), selector, cases)

  def InlineMatch(selector: Tree, cases: List[CaseDef])(implicit ctx: Context): Match =
    ta.assignType(untpd.InlineMatch(selector, cases), selector, cases)

  def Labeled(bind: Bind, expr: Tree)(implicit ctx: Context): Labeled =
    ta.assignType(untpd.Labeled(bind, expr))

  def Labeled(sym: TermSymbol, expr: Tree)(implicit ctx: Context): Labeled =
    Labeled(Bind(sym, EmptyTree), expr)

  def Return(expr: Tree, from: Tree)(implicit ctx: Context): Return =
    ta.assignType(untpd.Return(expr, from))

  def Return(expr: Tree, from: Symbol)(implicit ctx: Context): Return =
    Return(expr, Ident(from.termRef))

  def WhileDo(cond: Tree, body: Tree)(implicit ctx: Context): WhileDo =
    ta.assignType(untpd.WhileDo(cond, body))

  def Try(block: Tree, cases: List[CaseDef], finalizer: Tree)(implicit ctx: Context): Try =
    ta.assignType(untpd.Try(block, cases, finalizer), block, cases)

  def SeqLiteral(elems: List[Tree], elemtpt: Tree)(implicit ctx: Context): SeqLiteral =
    ta.assignType(untpd.SeqLiteral(elems, elemtpt), elems, elemtpt)

  def JavaSeqLiteral(elems: List[Tree], elemtpt: Tree)(implicit ctx: Context): JavaSeqLiteral =
    ta.assignType(new untpd.JavaSeqLiteral(elems, elemtpt), elems, elemtpt).asInstanceOf[JavaSeqLiteral]

  def Inlined(call: Tree, bindings: List[MemberDef], expansion: Tree)(implicit ctx: Context): Inlined =
    ta.assignType(untpd.Inlined(call, bindings, expansion), bindings, expansion)

  def TypeTree(tp: Type)(implicit ctx: Context): TypeTree =
    untpd.TypeTree().withType(tp)

  def SingletonTypeTree(ref: Tree)(implicit ctx: Context): SingletonTypeTree =
    ta.assignType(untpd.SingletonTypeTree(ref), ref)

  def RefinedTypeTree(parent: Tree, refinements: List[Tree], refineCls: ClassSymbol)(implicit ctx: Context): Tree =
    ta.assignType(untpd.RefinedTypeTree(parent, refinements), parent, refinements, refineCls)

  def AppliedTypeTree(tycon: Tree, args: List[Tree])(implicit ctx: Context): AppliedTypeTree =
    ta.assignType(untpd.AppliedTypeTree(tycon, args), tycon, args)

  def ByNameTypeTree(result: Tree)(implicit ctx: Context): ByNameTypeTree =
    ta.assignType(untpd.ByNameTypeTree(result), result)

  def LambdaTypeTree(tparams: List[TypeDef], body: Tree)(implicit ctx: Context): LambdaTypeTree =
    ta.assignType(untpd.LambdaTypeTree(tparams, body), tparams, body)

  def MatchTypeTree(bound: Tree, selector: Tree, cases: List[CaseDef])(implicit ctx: Context): MatchTypeTree =
    ta.assignType(untpd.MatchTypeTree(bound, selector, cases), bound, selector, cases)

  def TypeBoundsTree(lo: Tree, hi: Tree)(implicit ctx: Context): TypeBoundsTree =
    ta.assignType(untpd.TypeBoundsTree(lo, hi), lo, hi)

  def Bind(sym: Symbol, body: Tree)(implicit ctx: Context): Bind =
    ta.assignType(untpd.Bind(sym.name, body), sym)

  /** A pattern corresponding to `sym: tpe` */
  def BindTyped(sym: TermSymbol, tpe: Type)(implicit ctx: Context): Bind =
    Bind(sym, Typed(Underscore(tpe), TypeTree(tpe)))

  def Alternative(trees: List[Tree])(implicit ctx: Context): Alternative =
    ta.assignType(untpd.Alternative(trees), trees)

  def UnApply(fun: Tree, implicits: List[Tree], patterns: List[Tree], proto: Type)(implicit ctx: Context): UnApply = {
    assert(fun.isInstanceOf[RefTree] || fun.isInstanceOf[GenericApply[_]])
    ta.assignType(untpd.UnApply(fun, implicits, patterns), proto)
  }

  def ValDef(sym: TermSymbol, rhs: LazyTree = EmptyTree)(implicit ctx: Context): ValDef =
    ta.assignType(untpd.ValDef(sym.name, TypeTree(sym.info), rhs), sym)

  def SyntheticValDef(name: TermName, rhs: Tree)(implicit ctx: Context): ValDef =
    ValDef(ctx.newSymbol(ctx.owner, name, Synthetic, rhs.tpe.widen, coord = rhs.span), rhs)

  def DefDef(sym: TermSymbol, tparams: List[TypeSymbol], vparamss: List[List[TermSymbol]],
             resultType: Type, rhs: Tree)(implicit ctx: Context): DefDef =
    ta.assignType(
      untpd.DefDef(sym.name, tparams map TypeDef, vparamss.nestedMap(ValDef(_)),
                   TypeTree(resultType), rhs),
      sym)

  def DefDef(sym: TermSymbol, rhs: Tree = EmptyTree)(implicit ctx: Context): DefDef =
    ta.assignType(DefDef(sym, Function.const(rhs) _), sym)

  def DefDef(sym: TermSymbol, rhsFn: List[List[Tree]] => Tree)(implicit ctx: Context): DefDef =
    polyDefDef(sym, Function.const(rhsFn))

  def polyDefDef(sym: TermSymbol, rhsFn: List[Type] => List[List[Tree]] => Tree)(implicit ctx: Context): DefDef = {
    val (tparams, mtp) = sym.info match {
      case tp: PolyType =>
        val tparams = ctx.newTypeParams(sym, tp.paramNames, EmptyFlags, tp.instantiateParamInfos(_))
        (tparams, tp.instantiate(tparams map (_.typeRef)))
      case tp => (Nil, tp)
    }

    def valueParamss(tp: Type): (List[List[TermSymbol]], Type) = tp match {
      case tp: MethodType =>
        val isParamDependent = tp.isParamDependent
        val previousParamRefs = if (isParamDependent) new mutable.ListBuffer[TermRef]() else null

        def valueParam(name: TermName, origInfo: Type): TermSymbol = {
          val maybeImplicit =
            if (tp.isContextual) Implicit | Given
            else if (tp.isImplicitMethod) Implicit
            else EmptyFlags
          val maybeErased = if (tp.isErasedMethod) Erased else EmptyFlags

          def makeSym(info: Type) = ctx.newSymbol(sym, name, TermParam | maybeImplicit | maybeErased, info, coord = sym.coord)

          if (isParamDependent) {
            val sym = makeSym(origInfo.substParams(tp, previousParamRefs.toList))
            previousParamRefs += sym.termRef
            sym
          }
          else
            makeSym(origInfo)
        }

        val params = (tp.paramNames, tp.paramInfos).zipped.map(valueParam)
        val (paramss, rtp) = valueParamss(tp.instantiate(params map (_.termRef)))
        (params :: paramss, rtp)
      case tp => (Nil, tp.widenExpr)
    }
    val (vparamss, rtp) = valueParamss(mtp)
    val targs = tparams map (_.typeRef)
    val argss = vparamss.nestedMap(vparam => Ident(vparam.termRef))
    DefDef(sym, tparams, vparamss, rtp, rhsFn(targs)(argss))
  }

  def TypeDef(sym: TypeSymbol)(implicit ctx: Context): TypeDef =
    ta.assignType(untpd.TypeDef(sym.name, TypeTree(sym.info)), sym)

  def ClassDef(cls: ClassSymbol, constr: DefDef, body: List[Tree], superArgs: List[Tree] = Nil)(implicit ctx: Context): TypeDef = {
    val firstParent :: otherParents = cls.info.parents
    val superRef =
      if (cls is Trait) TypeTree(firstParent)
      else {
        def isApplicable(ctpe: Type): Boolean = ctpe match {
          case ctpe: PolyType =>
            isApplicable(ctpe.instantiate(firstParent.argTypes))
          case ctpe: MethodType =>
            (superArgs corresponds ctpe.paramInfos)(_.tpe <:< _)
          case _ =>
            false
        }
        val constr = firstParent.decl(nme.CONSTRUCTOR).suchThat(constr => isApplicable(constr.info))
        New(firstParent, constr.symbol.asTerm, superArgs)
      }
      ClassDefWithParents(cls, constr, superRef :: otherParents.map(TypeTree(_)), body)
    }

  def ClassDefWithParents(cls: ClassSymbol, constr: DefDef, parents: List[Tree], body: List[Tree])(implicit ctx: Context): TypeDef = {
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
    val impl = untpd.Template(constr, parents, Nil, selfType, newTypeParams ++ body)
      .withType(localDummy.termRef)
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
      if (parents.head.classSymbol.is(Trait)) {
        val head = parents.head.parents.head
        if (head.isRef(defn.AnyClass)) defn.AnyRefType :: parents else head :: parents
      }
      else parents
    val cls = ctx.newNormalizedClassSymbol(owner, tpnme.ANON_CLASS, Synthetic | Final, parents1,
        coord = fns.map(_.span).reduceLeft(_ union _))
    val constr = ctx.newConstructor(cls, Synthetic, Nil, Nil).entered
    def forwarder(fn: TermSymbol, name: TermName) = {
      val fwdMeth = fn.copy(cls, name, Synthetic | Method | Final).entered.asTerm
      if (fwdMeth.allOverriddenSymbols.exists(!_.is(Deferred))) fwdMeth.setFlag(Override)
      polyDefDef(fwdMeth, tprefs => prefss => ref(fn).appliedToTypes(tprefs).appliedToArgss(prefss))
    }
    val forwarders = (fns, methNames).zipped.map(forwarder)
    val cdef = ClassDef(cls, DefDef(constr), forwarders)
    Block(cdef :: Nil, New(cls.typeRef, Nil))
  }

  def Import(importImplied: Boolean, expr: Tree, selectors: List[untpd.Tree])(implicit ctx: Context): Import =
    ta.assignType(untpd.Import(importImplied, expr, selectors), ctx.newImportSymbol(ctx.owner, expr))

  def PackageDef(pid: RefTree, stats: List[Tree])(implicit ctx: Context): PackageDef =
    ta.assignType(untpd.PackageDef(pid, stats), pid)

  def Annotated(arg: Tree, annot: Tree)(implicit ctx: Context): Annotated =
    ta.assignType(untpd.Annotated(arg, annot), arg, annot)

  def Throw(expr: Tree)(implicit ctx: Context): Tree =
    ref(defn.throwMethod).appliedTo(expr)

  // ------ Making references ------------------------------------------------------

  def prefixIsElidable(tp: NamedType)(implicit ctx: Context): Boolean = {
    val typeIsElidable = tp.prefix match {
      case pre: ThisType =>
        tp.isType ||
        pre.cls.isStaticOwner ||
        tp.symbol.isParamOrAccessor && !pre.cls.is(Trait) && ctx.owner.enclosingClass == pre.cls
          // was ctx.owner.enclosingClass.derivesFrom(pre.cls) which was not tight enough
          // and was spuriously triggered in case inner class would inherit from outer one
          // eg anonymous TypeMap inside TypeMap.andThen
      case pre: TermRef =>
        pre.symbol.is(Module) && pre.symbol.isStatic
      case pre =>
        pre `eq` NoPrefix
    }
    typeIsElidable ||
    tp.symbol.is(JavaStatic) ||
    tp.symbol.hasAnnotation(defn.ScalaStaticAnnot)
  }

  def needsSelect(tp: Type)(implicit ctx: Context): Boolean = tp match {
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
    else {
      val pre = tp.prefix
      if (pre.isSingleton) followOuterLinks(singleton(pre.dealias)).select(tp)
      else Select(TypeTree(pre), tp)
    }

  def ref(sym: Symbol)(implicit ctx: Context): Tree =
    ref(NamedType(sym.owner.thisType, sym.name, sym.denot))

  private def followOuterLinks(t: Tree)(implicit ctx: Context) = t match {
    case t: This if ctx.erasedTypes && !(t.symbol == ctx.owner.enclosingClass || t.symbol.isStaticOwner) =>
      // after erasure outer paths should be respected
      new ExplicitOuter.OuterOps(ctx).path(toCls = t.tpe.widen.classSymbol)
    case t =>
      t
  }

  def singleton(tp: Type)(implicit ctx: Context): Tree = tp match {
    case tp: TermRef => ref(tp)
    case tp: ThisType => This(tp.cls)
    case tp: SkolemType => singleton(tp.narrow)
    case SuperType(qual, _) => singleton(qual)
    case ConstantType(value) => Literal(value)
  }

  /** A tree representing a `newXYZArray` operation of the right
   *  kind for the given element type in `elemTpe`. No type arguments or
   *  `length` arguments are given.
   */
  def newArray(elemTpe: Type, returnTpe: Type, span: Span, dims: JavaSeqLiteral)(implicit ctx: Context): Tree = {
    val elemClass = elemTpe.classSymbol
    def newArr =
      ref(defn.DottyArraysModule).select(defn.newArrayMethod).withSpan(span)

    if (!ctx.erasedTypes) {
      assert(!TypeErasure.isGeneric(elemTpe), elemTpe) //needs to be done during typer. See Applications.convertNewGenericArray
      newArr.appliedToTypeTrees(TypeTree(returnTpe) :: Nil).appliedToArgs(clsOf(elemTpe) :: clsOf(returnTpe) :: dims :: Nil).withSpan(span)
    } else  // after erasure
      newArr.appliedToArgs(clsOf(elemTpe) :: clsOf(returnTpe) :: dims :: Nil).withSpan(span)
  }

  /** The wrapped array method name for an array of type elemtp */
  def wrapArrayMethodName(elemtp: Type)(implicit ctx: Context): TermName = {
    val elemCls = elemtp.classSymbol
    if (elemCls.isPrimitiveValueClass) nme.wrapXArray(elemCls.name)
    else if (elemCls.derivesFrom(defn.ObjectClass) && !elemCls.isNotRuntimeClass) nme.wrapRefArray
    else nme.genericWrapArray
  }

  /** A tree representing a `wrapXYZArray(tree)` operation of the right
   *  kind for the given element type in `elemTpe`.
   */
  def wrapArray(tree: Tree, elemtp: Type)(implicit ctx: Context): Tree = {
    ref(defn.getWrapVarargsArrayModule)
      .select(wrapArrayMethodName(elemtp))
      .appliedToTypes(if (elemtp.isPrimitiveValueType) Nil else elemtp :: Nil)
      .appliedTo(tree)
  }

  // ------ Creating typed equivalents of trees that exist only in untyped form -------

  /** new C(args), calling the primary constructor of C */
  def New(tp: Type, args: List[Tree])(implicit ctx: Context): Apply =
    New(tp, tp.dealias.typeSymbol.primaryConstructor.asTerm, args)

  /** new C(args), calling given constructor `constr` of C */
  def New(tp: Type, constr: TermSymbol, args: List[Tree])(implicit ctx: Context): Apply = {
    val targs = tp.argTypes
    val tycon = tp.typeConstructor
    New(tycon)
      .select(TermRef(tycon, constr))
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
  def Underscore(tp: Type)(implicit ctx: Context): Ident = untpd.Ident(nme.WILDCARD).withType(tp)

  def defaultValue(tpe: Type)(implicit ctx: Context): Tree = {
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

  override val cpy: TypedTreeCopier = // Type ascription needed to pick up any new members in TreeCopier (currently there are none)
    new TypedTreeCopier

  val cpyBetweenPhases: TimeTravellingTreeCopier = new TimeTravellingTreeCopier

  class TypedTreeCopier extends TreeCopier {
    def postProcess(tree: Tree, copied: untpd.Tree): copied.ThisTree[Type] =
      copied.withTypeUnchecked(tree.tpe)
    def postProcess(tree: Tree, copied: untpd.MemberDef): copied.ThisTree[Type] =
      copied.withTypeUnchecked(tree.tpe)

    protected val untpdCpy = untpd.cpy

    override def Select(tree: Tree)(qualifier: Tree, name: Name)(implicit ctx: Context): Select = {
      val tree1 = untpdCpy.Select(tree)(qualifier, name)
      tree match {
        case tree: Select if qualifier.tpe eq tree.qualifier.tpe =>
          tree1.withTypeUnchecked(tree.tpe)
        case _ =>
          val tree2 = tree.tpe match {
            case tpe: NamedType => tree1.withType(tpe.derivedSelect(qualifier.tpe.widenIfUnstable))
            case _ => tree1.withTypeUnchecked(tree.tpe)
          }
          ConstFold(tree2)
      }
    }

    override def Apply(tree: Tree)(fun: Tree, args: List[Tree])(implicit ctx: Context): Apply = {
      val tree1 = untpdCpy.Apply(tree)(fun, args)
      tree match {
        case tree: Apply
        if (fun.tpe eq tree.fun.tpe) && sameTypes(args, tree.args) =>
          tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, fun, args)
      }
    }

    override def TypeApply(tree: Tree)(fun: Tree, args: List[Tree])(implicit ctx: Context): TypeApply = {
      val tree1 = untpdCpy.TypeApply(tree)(fun, args)
      tree match {
        case tree: TypeApply
        if (fun.tpe eq tree.fun.tpe) && sameTypes(args, tree.args) =>
          tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, fun, args)
      }
    }

    override def Literal(tree: Tree)(const: Constant)(implicit ctx: Context): Literal =
      ta.assignType(untpdCpy.Literal(tree)(const))

    override def New(tree: Tree)(tpt: Tree)(implicit ctx: Context): New =
      ta.assignType(untpdCpy.New(tree)(tpt), tpt)

    override def Typed(tree: Tree)(expr: Tree, tpt: Tree)(implicit ctx: Context): Typed =
      ta.assignType(untpdCpy.Typed(tree)(expr, tpt), tpt)

    override def NamedArg(tree: Tree)(name: Name, arg: Tree)(implicit ctx: Context): NamedArg =
      ta.assignType(untpdCpy.NamedArg(tree)(name, arg), arg)

    override def Assign(tree: Tree)(lhs: Tree, rhs: Tree)(implicit ctx: Context): Assign =
      ta.assignType(untpdCpy.Assign(tree)(lhs, rhs))

    override def Block(tree: Tree)(stats: List[Tree], expr: Tree)(implicit ctx: Context): Block = {
      val tree1 = untpdCpy.Block(tree)(stats, expr)
      tree match {
        case tree: Block if (expr.tpe eq tree.expr.tpe) && (expr.tpe eq tree.tpe) =>
          // The last guard is a conservative check: if `tree.tpe` is different from `expr.tpe`, then
          // it was computed from widening `expr.tpe`, and tree transforms might cause `expr.tpe.widen`
          // to change even if `expr.tpe` itself didn't change, e.g:
          //     { val s = ...;  s }
          // If the type of `s` changed, then the type of the block might have changed, even though `expr.tpe`
          // will still be `TermRef(NoPrefix, s)`
          tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, stats, expr)
      }
    }

    override def If(tree: Tree)(cond: Tree, thenp: Tree, elsep: Tree)(implicit ctx: Context): If = {
      val tree1 = untpdCpy.If(tree)(cond, thenp, elsep)
      tree match {
        case tree: If if (thenp.tpe eq tree.thenp.tpe) && (elsep.tpe eq tree.elsep.tpe) &&
          ((tree.tpe eq thenp.tpe) || (tree.tpe eq elsep.tpe)) =>
          // The last guard is a conservative check similar to the one done in `Block` above,
          // if `tree.tpe` is not identical to the type of one of its branch, it might have been
          // computed from the widened type of the branches, so the same reasoning than
          // in `Block` applies.
          tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, thenp, elsep)
      }
    }

    override def Closure(tree: Tree)(env: List[Tree], meth: Tree, tpt: Tree)(implicit ctx: Context): Closure = {
      val tree1 = untpdCpy.Closure(tree)(env, meth, tpt)
      tree match {
        case tree: Closure if sameTypes(env, tree.env) && (meth.tpe eq tree.meth.tpe) && (tpt.tpe eq tree.tpt.tpe) =>
          tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, meth, tpt)
      }
    }

    override def Match(tree: Tree)(selector: Tree, cases: List[CaseDef])(implicit ctx: Context): Match = {
      val tree1 = untpdCpy.Match(tree)(selector, cases)
      tree match {
        case tree: Match if sameTypes(cases, tree.cases) => tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, selector, cases)
      }
    }

    override def CaseDef(tree: Tree)(pat: Tree, guard: Tree, body: Tree)(implicit ctx: Context): CaseDef = {
      val tree1 = untpdCpy.CaseDef(tree)(pat, guard, body)
      tree match {
        case tree: CaseDef if body.tpe eq tree.body.tpe => tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, pat, body)
      }
    }

    override def Labeled(tree: Tree)(bind: Bind, expr: Tree)(implicit ctx: Context): Labeled =
      ta.assignType(untpdCpy.Labeled(tree)(bind, expr))

    override def Return(tree: Tree)(expr: Tree, from: Tree)(implicit ctx: Context): Return =
      ta.assignType(untpdCpy.Return(tree)(expr, from))

    override def WhileDo(tree: Tree)(cond: Tree, body: Tree)(implicit ctx: Context): WhileDo =
      ta.assignType(untpdCpy.WhileDo(tree)(cond, body))

    override def Try(tree: Tree)(expr: Tree, cases: List[CaseDef], finalizer: Tree)(implicit ctx: Context): Try = {
      val tree1 = untpdCpy.Try(tree)(expr, cases, finalizer)
      tree match {
        case tree: Try if (expr.tpe eq tree.expr.tpe) && sameTypes(cases, tree.cases) => tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, expr, cases)
      }
    }

    override def Inlined(tree: Tree)(call: Tree, bindings: List[MemberDef], expansion: Tree)(implicit ctx: Context): Inlined = {
      val tree1 = untpdCpy.Inlined(tree)(call, bindings, expansion)
      tree match {
        case tree: Inlined if sameTypes(bindings, tree.bindings) && (expansion.tpe eq tree.expansion.tpe) =>
          tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, bindings, expansion)
      }
    }

    override def SeqLiteral(tree: Tree)(elems: List[Tree], elemtpt: Tree)(implicit ctx: Context): SeqLiteral = {
      val tree1 = untpdCpy.SeqLiteral(tree)(elems, elemtpt)
      tree match {
        case tree: SeqLiteral
        if sameTypes(elems, tree.elems) && (elemtpt.tpe eq tree.elemtpt.tpe) =>
          tree1.withTypeUnchecked(tree.tpe)
        case _ =>
          ta.assignType(tree1, elems, elemtpt)
      }
    }

    override def Annotated(tree: Tree)(arg: Tree, annot: Tree)(implicit ctx: Context): Annotated = {
      val tree1 = untpdCpy.Annotated(tree)(arg, annot)
      tree match {
        case tree: Annotated if (arg.tpe eq tree.arg.tpe) && (annot eq tree.annot) => tree1.withTypeUnchecked(tree.tpe)
        case _ => ta.assignType(tree1, arg, annot)
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

  class TimeTravellingTreeCopier extends TypedTreeCopier {
    override def Apply(tree: Tree)(fun: Tree, args: List[Tree])(implicit ctx: Context): Apply =
      ta.assignType(untpdCpy.Apply(tree)(fun, args), fun, args)
      // Note: Reassigning the original type if `fun` and `args` have the same types as before
      // does not work here: The computed type depends on the widened function type, not
      // the function type itself. A treetransform may keep the function type the
      // same but its widened type might change.

    override def TypeApply(tree: Tree)(fun: Tree, args: List[Tree])(implicit ctx: Context): TypeApply =
      ta.assignType(untpdCpy.TypeApply(tree)(fun, args), fun, args)
      // Same remark as for Apply

    override def Closure(tree: Tree)(env: List[Tree], meth: Tree, tpt: Tree)(implicit ctx: Context): Closure =
            ta.assignType(untpdCpy.Closure(tree)(env, meth, tpt), meth, tpt)

    override def Closure(tree: Closure)(env: List[Tree] = tree.env, meth: Tree = tree.meth, tpt: Tree = tree.tpt)(implicit ctx: Context): Closure =
      Closure(tree: Tree)(env, meth, tpt)
  }

  override def skipTransform(tree: Tree)(implicit ctx: Context): Boolean = tree.tpe.isError

  implicit class TreeOps[ThisTree <: tpd.Tree](private val tree: ThisTree) extends AnyVal {

    def isValue(implicit ctx: Context): Boolean =
      tree.isTerm && tree.tpe.widen.isValueType

    def isValueOrPattern(implicit ctx: Context): Boolean =
      tree.isValue || tree.isPattern

    def isValueType: Boolean =
      tree.isType && tree.tpe.isValueType

    def isInstantiation: Boolean = tree match {
      case Apply(Select(New(_), nme.CONSTRUCTOR), _) => true
      case _ => false
    }

    def shallowFold[T](z: T)(op: (T, tpd.Tree) => T)(implicit ctx: Context): T =
      new ShallowFolder(op).apply(z, tree)

    def deepFold[T](z: T)(op: (T, tpd.Tree) => T)(implicit ctx: Context): T =
      new DeepFolder(op).apply(z, tree)

    def find[T](pred: (tpd.Tree) => Boolean)(implicit ctx: Context): Option[tpd.Tree] =
      shallowFold[Option[tpd.Tree]](None)((accum, tree) => if (pred(tree)) Some(tree) else accum)

    def subst(from: List[Symbol], to: List[Symbol])(implicit ctx: Context): ThisTree =
      new TreeTypeMap(substFrom = from, substTo = to).apply(tree)

    /** Change owner from `from` to `to`. If `from` is a weak owner, also change its
     *  owner to `to`, and continue until a non-weak owner is reached.
     */
    def changeOwner(from: Symbol, to: Symbol)(implicit ctx: Context): ThisTree = {
      @tailrec def loop(from: Symbol, froms: List[Symbol], tos: List[Symbol]): ThisTree = {
        if (from.isWeakOwner && !from.owner.isClass)
          loop(from.owner, from :: froms, to :: tos)
        else {
          //println(i"change owner ${from :: froms}%, % ==> $tos of $tree")
          new TreeTypeMap(oldOwners = from :: froms, newOwners = tos).apply(tree)
        }
      }
      if (from == to) tree else loop(from, Nil, to :: Nil)
    }

    /**
     * Set the owner of every definition in this tree which is not itself contained in this
     * tree to be `newowner`
     */
    def changeNonLocalOwners(newOwner: Symbol)(implicit ctx: Context): Tree = {
      val ownerAcc = new TreeAccumulator[immutable.Set[Symbol]] {
        def apply(ss: immutable.Set[Symbol], tree: Tree)(implicit ctx: Context) = tree match {
          case tree: DefTree =>
            if (tree.symbol.exists) ss + tree.symbol.owner
            else ss
          case _ =>
            foldOver(ss, tree)
        }
      }
      val owners = ownerAcc(immutable.Set.empty[Symbol], tree).toList
      val newOwners = List.fill(owners.size)(newOwner)
      new TreeTypeMap(oldOwners = owners, newOwners = newOwners).apply(tree)
    }

    /** After phase `trans`, set the owner of every definition in this tree that was formerly
     *  owner by `from` to `to`.
     */
    def changeOwnerAfter(from: Symbol, to: Symbol, trans: DenotTransformer)(implicit ctx: Context): ThisTree =
      if (ctx.phase == trans.next) {
        val traverser = new TreeTraverser {
          def traverse(tree: Tree)(implicit ctx: Context) = tree match {
            case tree: DefTree =>
              val sym = tree.symbol
              val prevDenot = sym.denot(ctx.withPhase(trans))
              if (prevDenot.effectiveOwner == from.skipWeakOwner) {
                val d = sym.copySymDenotation(owner = to)
                d.installAfter(trans)
                d.transformAfter(trans, d => if (d.owner eq from) d.copySymDenotation(owner = to) else d)
              }
              if (sym.isWeakOwner) traverseChildren(tree)
            case _ =>
              traverseChildren(tree)
          }
        }
        traverser.traverse(tree)
        tree
      }
      else changeOwnerAfter(from, to, trans)(ctx.withPhase(trans.next))

    /** A select node with the given selector name and a computed type */
    def select(name: Name)(implicit ctx: Context): Select =
      Select(tree, name)

    /** A select node with the given selector name such that the designated
     *  member satisfies predicate `p`. Useful for disambiguating overloaded members.
     */
    def select(name: Name, p: Symbol => Boolean)(implicit ctx: Context): Select =
      select(tree.tpe.member(name).suchThat(p).symbol)

    /** A select node with the given type */
    def select(tp: NamedType)(implicit ctx: Context): Select =
      untpd.Select(tree, tp.name).withType(tp)

    /** A select node that selects the given symbol. Note: Need to make sure this
     *  is in fact the symbol you would get when you select with the symbol's name,
     *  otherwise a data race may occur which would be flagged by -Yno-double-bindings.
     */
    def select(sym: Symbol)(implicit ctx: Context): Select = {
      val tp =
        if (sym.isType) {
          assert(!sym.is(TypeParam))
          TypeRef(tree.tpe, sym.asType)
        }
        else
          TermRef(tree.tpe, sym.name.asTermName, sym.denot.asSeenFrom(tree.tpe))
      untpd.Select(tree, sym.name).withType(tp)
    }

    /** A select node with the given selector name and signature and a computed type */
    def selectWithSig(name: Name, sig: Signature)(implicit ctx: Context): Tree =
      untpd.SelectWithSig(tree, name, sig).withType(tree.tpe.select(name.asTermName, sig))

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

    /** `tree == that` */
    def equal(that: Tree)(implicit ctx: Context): Tree =
      if (that.tpe.widen.isRef(defn.NothingClass))
        Literal(Constant(false))
      else
        applyOverloaded(tree, nme.EQ, that :: Nil, Nil, defn.BooleanType)

    /** `tree.isInstanceOf[tp]`, with special treatment of singleton types */
    def isInstance(tp: Type)(implicit ctx: Context): Tree = tp.dealias match {
      case tp: SingletonType =>
        if (tp.widen.derivesFrom(defn.ObjectClass))
          tree.ensureConforms(defn.ObjectType).select(defn.Object_eq).appliedTo(singleton(tp))
        else
          singleton(tp).equal(tree)
      case _ =>
        tree.select(defn.Any_isInstanceOf).appliedToType(tp)
    }

    /** tree.asInstanceOf[`tp`] */
    def asInstance(tp: Type)(implicit ctx: Context): Tree = {
      assert(tp.isValueType, i"bad cast: $tree.asInstanceOf[$tp]")
      tree.select(defn.Any_asInstanceOf).appliedToType(tp)
    }

    /** cast tree to `tp`, assuming no exception is raised, i.e the operation is pure */
    def cast(tp: Type)(implicit ctx: Context): Tree = {
      assert(tp.isValueType, i"bad cast: $tree.asInstanceOf[$tp]")
      tree.select(if (ctx.erasedTypes) defn.Any_asInstanceOf else defn.Any_typeCast)
        .appliedToType(tp)
    }

    /** cast `tree` to `tp` (or its box/unbox/cast equivalent when after
     *  erasure and value and non-value types are mixed),
     *  unless tree's type already conforms to `tp`.
     */
    def ensureConforms(tp: Type)(implicit ctx: Context): Tree =
      if (tree.tpe <:< tp) tree
      else if (!ctx.erasedTypes) cast(tp)
      else Erasure.Boxing.adaptToType(tree, tp)

    /** `tree ne null` (might need a cast to be type correct) */
    def testNotNull(implicit ctx: Context): Tree = {
      val receiver = if (defn.isBottomType(tree.tpe)) {
        // If the receiver is of type `Nothing` or `Null`, add an ascription so that the selection
        // succeeds: e.g. `null.ne(null)` doesn't type, but `(null: AnyRef).ne(null)` does.
        Typed(tree, TypeTree(defn.AnyRefType))
      }
      else tree.ensureConforms(defn.ObjectType)
      receiver.select(defn.Object_ne).appliedTo(Literal(Constant(null)))
    }

    /** If inititializer tree is `_', the default value of its type,
     *  otherwise the tree itself.
     */
    def wildcardToDefault(implicit ctx: Context): Tree =
      if (isWildcardArg(tree)) defaultValue(tree.tpe) else tree

    /** `this && that`, for boolean trees `this`, `that` */
    def and(that: Tree)(implicit ctx: Context): Tree =
      tree.select(defn.Boolean_&&).appliedTo(that)

    /** `this || that`, for boolean trees `this`, `that` */
    def or(that: Tree)(implicit ctx: Context): Tree =
      tree.select(defn.Boolean_||).appliedTo(that)

    /** The translation of `tree = rhs`.
     *  This is either the tree as an assignment, or a setter call.
     */
    def becomes(rhs: Tree)(implicit ctx: Context): Tree = {
      val sym = tree.symbol
      if (sym is Method) {
        val setter = sym.setter.orElse {
          assert(sym.name.isSetterName && sym.info.firstParamTypes.nonEmpty)
          sym
        }
        val qual = tree match {
          case id: Ident => desugarIdentPrefix(id)
          case Select(qual, _) => qual
        }
        qual.select(setter).appliedTo(rhs)
      }
      else Assign(tree, rhs)
    }

    /** tree @annot
     *
     *  works differently for type trees and term trees
     */
    def annotated(annot: Tree)(implicit ctx: Context): Tree =
      if (tree.isTerm)
        Typed(tree, TypeTree(AnnotatedType(tree.tpe.widenIfUnstable, Annotation(annot))))
      else
        Annotated(tree, annot)

    /** A synthetic select with that will be turned into an outer path by ExplicitOuter.
     *  @param levels  How many outer levels to select
     *  @param tp      The type of the destination of the outer path.
     */
    def outerSelect(levels: Int, tp: Type)(implicit ctx: Context): Tree =
      untpd.Select(tree, OuterSelectName(EmptyTermName, levels)).withType(SkolemType(tp))

    /** Replace Inlined nodes and InlineProxy references to underlying arguments */
    def underlyingArgument(implicit ctx: Context): Tree = {
      val mapToUnderlying = new MapToUnderlying {
        /** Should get the rhs of this binding
         *  Returns true if the symbol is a val or def generated by eta-expansion/inline
         */
        override protected def skipLocal(sym: Symbol): Boolean =
          sym.is(InlineProxy) || sym.is(Synthetic)
      }
      mapToUnderlying.transform(tree)
    }

    /** Replace Ident nodes references to the underlying tree that defined them */
    def underlying(implicit ctx: Context): Tree = new MapToUnderlying().transform(tree)

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

    /** Set this tree as the `defTree` of its symbol and return this tree */
    def setDefTree(implicit ctx: Context): ThisTree = {
      val sym = tree.symbol
      if (sym.exists) sym.defTree = tree
      tree
    }
  }

  /** Map Inlined nodes, NamedArgs, Blocks with no statements and local references to underlying arguments.
   *  Also drops Inline and Block with no statements.
   */
  class MapToUnderlying extends TreeMap {
    override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
      case tree: Ident if isBinding(tree.symbol) && skipLocal(tree.symbol) =>
        tree.symbol.defTree match {
          case defTree: ValOrDefDef =>
            val rhs = defTree.rhs
            assert(!rhs.isEmpty)
            transform(rhs)
          case _ => tree
        }
      case Inlined(_, _, arg) => transform(arg)
      case Block(Nil, arg) => transform(arg)
      case NamedArg(_, arg) => transform(arg)
      case tree => super.transform(tree)
    }

    /** Should get the rhs of this binding */
    protected def skipLocal(sym: Symbol): Boolean = true

    /** Is this a symbol that of a local val or parameterless def for which we could get the rhs */
    private def isBinding(sym: Symbol)(implicit ctx: Context): Boolean = {
      sym.isTerm && !sym.is(Param) && !sym.owner.isClass &&
      !(sym.is(Method) && sym.info.isInstanceOf[MethodOrPoly]) // if is a method it is parameterless
    }
  }

  implicit class ListOfTreeDecorator(val xs: List[tpd.Tree]) extends AnyVal {
    def tpes: List[Type] = xs match {
      case x :: xs1 => x.tpe :: xs1.tpes
      case nil => Nil
    }
  }

  /** A trait for loaders that compute trees. Currently implemented just by DottyUnpickler. */
  trait TreeProvider {
    protected def computeRootTrees(implicit ctx: Context): List[Tree]

    private[this] var myTrees: List[Tree] = null

    /** Get trees defined by this provider. Cache them if -Yretain-trees is set. */
    def rootTrees(implicit ctx: Context): List[Tree] =
      if (ctx.settings.YretainTrees.value) {
        if (myTrees == null) myTrees = computeRootTrees
        myTrees
      } else computeRootTrees

    /** Get first tree defined by this provider, or EmptyTree if none exists */
    def tree(implicit ctx: Context): Tree =
      rootTrees.headOption.getOrElse(EmptyTree)

    /** Is it possible that the tree to load contains a definition of or reference to `id`? */
    def mightContain(id: String)(implicit ctx: Context): Boolean = true
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
      Throw(New(defn.ClassCastExceptionClass.typeRef, Nil)).withSpan(tree.span)
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

  def applyOverloaded(receiver: Tree, method: TermName, args: List[Tree], targs: List[Type],
                      expectedType: Type, isContextual: Boolean = false)(implicit ctx: Context): Tree = {
    val typer = ctx.typer
    val proto = new FunProtoTyped(args, expectedType)(typer, isContextual)
    val denot = receiver.tpe.member(method)
    assert(denot.exists, i"no member $receiver . $method, members = ${receiver.tpe.decls}")
    val selected =
      if (denot.isOverloaded) {
        def typeParamCount(tp: Type) = tp.widen match {
          case tp: PolyType => tp.paramInfos.length
          case _ => 0
        }
        var allAlts = denot.alternatives
          .map(denot => TermRef(receiver.tpe, denot.symbol))
          .filter(tr => typeParamCount(tr) == targs.length)
        if (targs.isEmpty) allAlts = allAlts.filterNot(_.widen.isInstanceOf[PolyType])
        val alternatives = ctx.typer.resolveOverloaded(allAlts, proto)
        assert(alternatives.size == 1,
          i"${if (alternatives.isEmpty) "no" else "multiple"} overloads available for " +
          i"$method on ${receiver.tpe.widenDealiasKeepAnnots} with targs: $targs%, %; args: $args%, % of types ${args.tpes}%, %; expectedType: $expectedType." +
          i"all alternatives: ${allAlts.map(_.symbol.showDcl).mkString(", ")}\n" +
          i"matching alternatives: ${alternatives.map(_.symbol.showDcl).mkString(", ")}.") // this is parsed from bytecode tree. there's nothing user can do about it
        alternatives.head
      }
      else TermRef(receiver.tpe, denot.symbol)
    val fun = receiver.select(selected).appliedToTypes(targs)

    val apply = untpd.Apply(fun, args)
    new typer.ApplyToTyped(apply, fun, selected, args, expectedType).result.asInstanceOf[Tree] // needed to handle varargs
  }

  @tailrec
  def sameTypes(trees: List[tpd.Tree], trees1: List[tpd.Tree]): Boolean = {
    if (trees.isEmpty) trees.isEmpty
    else if (trees1.isEmpty) trees.isEmpty
    else (trees.head.tpe eq trees1.head.tpe) && sameTypes(trees.tail, trees1.tail)
  }

  /** If `tree`'s purity level is less than `level`, let-bind it so that it gets evaluated
   *  only once. I.e. produce a
   *
   *     { val x = 'tree ;  ~within('x) }
   *
   *  instead of otherwise
   *
   *     ~within('tree)
   */
  def letBindUnless(level: TreeInfo.PurityLevel, tree: Tree)(within: Tree => Tree)(implicit ctx: Context): Tree = {
    if (exprPurity(tree) >= level) within(tree)
    else {
      val vdef = SyntheticValDef(TempResultName.fresh(), tree)
      Block(vdef :: Nil, within(Ident(vdef.namedType)))
    }
  }

  /** Let bind `tree` unless `tree` is at least idempotent */
  def evalOnce(tree: Tree)(within: Tree => Tree)(implicit ctx: Context): Tree =
    letBindUnless(TreeInfo.Idempotent, tree)(within)

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

  /** An extractor for typed splices */
  object Splice {
    def apply(tree: Tree)(implicit ctx: Context): Tree = {
      val baseType = tree.tpe.baseType(defn.QuotedExprClass)
      val argType =
        if (baseType != NoType) baseType.argTypesHi.head
        else {
          assert(ctx.reporter.hasErrors)
          defn.NothingType
        }
      ref(defn.InternalQuoted_exprSplice).appliedToType(argType).appliedTo(tree)
    }
    def unapply(tree: Tree)(implicit ctx: Context): Option[Tree] = tree match {
      case Apply(fn, arg :: Nil) if fn.symbol == defn.InternalQuoted_exprSplice => Some(arg)
      case _ => None
    }
  }

  /** A key to be used in a context property that tracks enclosing inlined calls */
  private val InlinedCalls = new Property.Key[List[Tree]]

  /** Record an enclosing inlined call.
   *  EmptyTree calls (for parameters) cancel the next-enclosing call in the list instead of being added to it.
   *  We assume parameters are never nested inside parameters.
   */
  override def inlineContext(call: Tree)(implicit ctx: Context): Context = {
    // We assume enclosingInlineds is already normalized, and only process the new call with the head.
    val oldIC = enclosingInlineds
    val newIC = (call, oldIC) match {
      case (t, t1 :: ts2) if t.isEmpty =>
        assert(!t1.isEmpty)
        ts2
      case _ => call :: oldIC
    }
    ctx.fresh.setProperty(InlinedCalls, newIC)
  }

  /** All enclosing calls that are currently inlined, from innermost to outermost.
   */
  def enclosingInlineds(implicit ctx: Context): List[Tree] =
    ctx.property(InlinedCalls).getOrElse(Nil)

  /** The source file where the symbol of the `inline` method referred to by `call`
   *  is defined
   */
  def sourceFile(call: Tree)(implicit ctx: Context): SourceFile = call.symbol.source

  /** Desugar identifier into a select node. Return the tree itself if not possible */
  def desugarIdent(tree: Ident)(implicit ctx: Context): Tree = {
    val qual = desugarIdentPrefix(tree)
    if (qual.isEmpty) tree
    else qual.select(tree.symbol)
  }

  /** Recover identifier prefix (e.g. this) if it exists */
  def desugarIdentPrefix(tree: Ident)(implicit ctx: Context): Tree = tree.tpe match {
    case TermRef(prefix: TermRef, _) =>
      ref(prefix)
    case TermRef(prefix: ThisType, _) =>
      This(prefix.cls)
    case _ =>
      EmptyTree
  }

  /**
   * The symbols that are imported with `expr.name`
   *
   * @param expr The base of the import statement
   * @param name The name that is being imported.
   * @return All the symbols that would be imported with `expr.name`.
   */
  def importedSymbols(expr: Tree, name: Name)(implicit ctx: Context): List[Symbol] = {
    def lookup(name: Name): Symbol = expr.tpe.member(name).symbol
    val symbols =
      List(lookup(name.toTermName),
           lookup(name.toTypeName),
           lookup(name.moduleClassName),
           lookup(name.sourceModuleName))

    symbols.map(_.sourceSymbol).filter(_.exists).distinct
  }

  /**
   * All the symbols that are imported by the first selector of `imp` that matches
   * `selectorPredicate`.
   *
   * @param imp The import statement to analyze
   * @param selectorPredicate A test to find the selector to use.
   * @return The symbols imported.
   */
  def importedSymbols(imp: Import,
                      selectorPredicate: untpd.Tree => Boolean = util.common.alwaysTrue)
                     (implicit ctx: Context): List[Symbol] = {
    imp.selectors.find(selectorPredicate) match {
      case Some(id: untpd.Ident) =>
        importedSymbols(imp.expr, id.name)
      case Some(Thicket((id: untpd.Ident) :: (_: untpd.Ident) :: Nil)) =>
        importedSymbols(imp.expr, id.name)
      case _ =>
        Nil
    }
  }

  /**
   * The list of select trees that resolve to the same symbols as the ones that are imported
   * by `imp`.
   */
  def importSelections(imp: Import)(implicit ctx: Context): List[Select] = {
    def imported(sym: Symbol, id: untpd.Ident, rename: Option[untpd.Ident]): List[Select] = {
      // Give a zero-extent position to the qualifier to prevent it from being included several
      // times in results in the language server.
      val noPosExpr = focusPositions(imp.expr)
      val selectTree = Select(noPosExpr, sym.name).withSpan(id.span)
      rename match {
        case None =>
          selectTree :: Nil
        case Some(rename) =>
          // Get the type of the symbol that is actually selected, and construct a select
          // node with the new name and the type of the real symbol.
          val name = if (sym.name.isTypeName) rename.name.toTypeName else rename.name
          val actual = Select(noPosExpr, sym.name)
          val renameTree = Select(noPosExpr, name).withSpan(rename.span).withType(actual.tpe)
          selectTree :: renameTree :: Nil
      }
    }

    imp.selectors.flatMap {
      case Ident(nme.WILDCARD) =>
        Nil
      case id: untpd.Ident =>
        importedSymbols(imp.expr, id.name).flatMap { sym =>
          imported(sym, id, None)
        }
      case Thicket((id: untpd.Ident) :: (newName: untpd.Ident) :: Nil) =>
        importedSymbols(imp.expr, id.name).flatMap { sym =>
          imported(sym, id, Some(newName))
        }
    }
  }

  /** Replaces all positions in `tree` with zero-extent positions */
  private def focusPositions(tree: Tree)(implicit ctx: Context): Tree = {
    val transformer = new tpd.TreeMap {
      override def transform(tree: Tree)(implicit ctx: Context): Tree = {
        super.transform(tree).withSpan(tree.span.focus)
      }
    }
    transformer.transform(tree)
  }
}

