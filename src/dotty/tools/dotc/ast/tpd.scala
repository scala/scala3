package dotty.tools
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._
import CheckTrees._, Denotations._
import config.Printers._

/** Some creators for typed trees */
object tpd extends Trees.Instance[Type] with TypedTreeInfo {

  def Modifiers(sym: Symbol)(implicit ctx: Context): Modifiers = Modifiers(
    sym.flags & ModifierFlags,
    if (sym.privateWithin.exists) sym.privateWithin.asType.name else tpnme.EMPTY,
    sym.annotations map (_.tree))

  def Ident(tp: NamedType)(implicit ctx: Context): Ident =
    underlyingIfRepeated(untpd.Ident(tp.name) withType tp).checked

  def Select(qualifier: Tree, name: Name)(implicit ctx: Context): Select =
    untpd.Select(qualifier, name).withType(qualifier.tpe select name)

  def Select(qualifier: Tree, tp: NamedType)(implicit ctx: Context): Select =
    untpd.Select(qualifier, tp.name).withType(tp).checked

  def SelectWithSig(qualifier: Tree, name: Name, sig: Signature)(implicit ctx: Context) =
    untpd.SelectWithSig(qualifier, name, sig)
      .withType(TermRef.withSig(qualifier.tpe, name.asTermName, sig))

  def This(cls: ClassSymbol)(implicit ctx: Context): This =
    untpd.This(cls.name).withType(cls.thisType).checked

  def Super(qual: Tree, mix: TypeName)(implicit ctx: Context): Super = {
    val owntype =
      if (mix.isEmpty) ctx.typeComparer.glb(qual.tpe.parents)
      else {
        val mixParents = qual.tpe.parents filter (_.name == mix)
        check(mixParents.length == 1)
        mixParents.head
      }
    untpd.Super(qual, mix).withType(SuperType(qual.tpe, owntype)).checked
  }

  def Apply(fn: Tree, args: List[Tree])(implicit ctx: Context): Apply = {
    val owntype = fn.tpe.widen match {
      case fntpe @ MethodType(pnames, ptypes) =>
        check(sameLength(ptypes, args), s"${fn.show}: ${fntpe.show} to ${args.map(_.show).mkString(", ")}")
        fntpe.instantiate(args map (_.tpe))
      case t =>
        check(false, s"fn = $fn, args = $args, tp = $t")
        ErrorType
    }
    untpd.Apply(fn, args).withType(owntype).checked
  }

  def TypeApply(fn: Tree, args: List[Tree])(implicit ctx: Context): TypeApply = {
    val owntype = fn.tpe.widen match {
      case fntpe @ PolyType(pnames) =>
        check(sameLength(pnames, args))
        fntpe.instantiate(args map (_.tpe))
      case t =>
        check(false, s"bad type: $t")
        ErrorType
    }
    untpd.TypeApply(fn, args).withType(owntype).checked
  }

  def Literal(const: Constant)(implicit ctx: Context): Literal =
    untpd.Literal(const).withType(const.tpe).checked

  def unitLiteral(implicit ctx: Context): Literal =
    Literal(Constant(()))

  def New(tpt: Tree)(implicit ctx: Context): New =
    untpd.New(tpt).withType(tpt.tpe).checked

  def New(tp: Type)(implicit ctx: Context): New = New(TypeTree(tp))

  def Pair(left: Tree, right: Tree)(implicit ctx: Context): Pair =
    untpd.Pair(left, right).withType(defn.PairType.appliedTo(left.tpe, right.tpe)).checked

  def Typed(expr: Tree, tpt: Tree)(implicit ctx: Context): Typed =
    untpd.Typed(expr, tpt).withType(tpt.tpe).checked

  def NamedArg(name: Name, arg: Tree)(implicit ctx: Context) =
    untpd.NamedArg(name, arg).withType(arg.tpe).checked

  def Assign(lhs: Tree, rhs: Tree)(implicit ctx: Context): Assign =
    untpd.Assign(lhs, rhs).withType(defn.UnitType).checked

  def Block(stats: List[Tree], expr: Tree)(implicit ctx: Context): Block =
    untpd.Block(stats, expr).withType(avoid(expr.tpe, localSyms(stats))).checked

  def avoid(tp: Type, syms: => List[Symbol])(implicit ctx: Context): Type = {
    val widenMap = new TypeMap {
      lazy val forbidden = syms.toSet
      def toAvoid(tp: Type): Boolean = tp match {
        case tp: TermRef =>
          val sym = tp.symbol
          sym.exists && (
               sym.owner.isTerm && (forbidden contains sym)
            || !(sym.owner is Package) && toAvoid(tp.prefix)
            )
        case _ =>
          false
      }
      def apply(tp: Type) = tp match {
        case tp: TermRef if toAvoid(tp) && variance > 0 =>
          apply(tp.info)
        case tp: TypeRef if toAvoid(tp.prefix) =>
          tp.info match {
            case TypeAlias(ref) => apply(ref)
            case _ => mapOver(tp)
          }
        case tp: RefinedType =>
          val tp1 @ RefinedType(parent1, _) = mapOver(tp)
          if (tp1.refinedInfo existsPart toAvoid) {
            typr.println(s"dropping refinement from $tp1")
            parent1
          }
          else tp1
        case _ =>
          mapOver(tp)
      }
    }
    widenMap(tp)
  }

  def maybeBlock(stats: List[Tree], expr: Tree)(implicit ctx: Context): Tree =
    if (stats.isEmpty) expr else Block(stats, expr)

  def If(cond: Tree, thenp: Tree, elsep: Tree)(implicit ctx: Context): If =
    untpd.If(cond, thenp, elsep).withType(thenp.tpe | elsep.tpe).checked

  def Closure(env: List[Tree], meth: Tree, tpt: Tree)(implicit ctx: Context): Closure = {
    val ownType = if (tpt.isEmpty) meth.tpe.widen.toFunctionType else tpt.tpe
    untpd.Closure(env, meth, tpt).withType(ownType).checked
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
  def Closure(meth: TermSymbol, rhsFn: List[List[Tree]] => Tree, targetType: Type = NoType)(implicit ctx: Context): Block = {
    val targetTpt = if (targetType.exists) TypeTree(targetType) else EmptyTree
    Block(
      DefDef(meth, rhsFn) :: Nil,
      Closure(Nil, Ident(TermRef(NoPrefix, meth)), targetTpt))
  }

  def Match(selector: Tree, cases: List[CaseDef])(implicit ctx: Context): Match =
    untpd.Match(selector, cases).withType(ctx.typeComparer.lub(cases.tpes)).checked

  def CaseDef(pat: Tree, guard: Tree, body: Tree)(implicit ctx: Context): CaseDef =
    untpd.CaseDef(pat, guard, body).withType(body.tpe).checked

  def Return(expr: Tree, from: Tree)(implicit ctx: Context): Return =
    untpd.Return(expr, from).withType(defn.NothingType).checked

  def Try(block: Tree, handler: Tree, finalizer: Tree)(implicit ctx: Context): Try =
    untpd.Try(block, handler, finalizer).withType(block.tpe | handler.tpe).checked

  def Throw(expr: Tree)(implicit ctx: Context): Throw =
    untpd.Throw(expr).withType(defn.NothingType).checked

  def SeqLiteral(elems: List[Tree])(implicit ctx: Context): SeqLiteral =
    untpd.SeqLiteral(elems)
      .withType(defn.SeqClass.typeRef.appliedTo(ctx.typeComparer.lub(elems.tpes)))
      .checked

  def SeqLiteral(tpe: Type, elems: List[Tree])(implicit ctx: Context): SeqLiteral = {
    val untpdSeqLit =
      if (tpe derivesFrom defn.SeqClass) untpd.SeqLiteral(elems)
      else untpd.JavaSeqLiteral(elems)
    untpdSeqLit.withType(tpe).checked
  }

  def JavaSeqLiteral(elems: List[Tree])(implicit ctx: Context): SeqLiteral =
    new untpd.JavaSeqLiteral(elems)
      .withType(defn.ArrayClass.typeRef.appliedTo(ctx.typeComparer.lub(elems.tpes)))
      .checked

  def TypeTree(original: Tree)(implicit ctx: Context): TypeTree =
    TypeTree(original.tpe, original)

  def TypeTree(tp: Type, original: Tree = EmptyTree)(implicit ctx: Context): TypeTree =
    untpd.TypeTree(original).withType(tp).checked

  def SingletonTypeTree(ref: Tree)(implicit ctx: Context): SingletonTypeTree =
    untpd.SingletonTypeTree(ref).withType(ref.tpe).checked

  def SelectFromTypeTree(qualifier: Tree, name: Name)(implicit ctx: Context): SelectFromTypeTree =
    untpd.SelectFromTypeTree(qualifier, name).withType(qualifier.tpe select name).checked

  def SelectFromTypeTree(qualifier: Tree, tp: NamedType)(implicit ctx: Context): SelectFromTypeTree =
    untpd.SelectFromTypeTree(qualifier, tp.name).withType(tp).checked

  def AndTypeTree(left: Tree, right: Tree)(implicit ctx: Context): AndTypeTree =
    untpd.AndTypeTree(left, right).withType(left.tpe & right.tpe).checked

  def OrTypeTree(left: Tree, right: Tree)(implicit ctx: Context): OrTypeTree =
    untpd.OrTypeTree(left, right).withType(left.tpe | right.tpe).checked

  def RefinedTypeTree(tpt: Tree, refinements: List[Tree])(implicit ctx: Context): RefinedTypeTree = {
    def refineType(tp: Type, refinement: Symbol): Type =
      RefinedType(tp, refinement.name, refinement.info)
    untpd.RefinedTypeTree(tpt, refinements)
      .withType((tpt.tpe /: (refinements map (_.symbol)))(refineType)).checked
  }

  def refineType(tp: Type, refinement: Symbol)(implicit ctx: Context): Type =
    RefinedType(tp, refinement.name, refinement.info)

  def AppliedTypeTree(tpt: Tree, args: List[Tree])(implicit ctx: Context): AppliedTypeTree =
    untpd.AppliedTypeTree(tpt, args).withType(tpt.tpe.appliedTo(args map (_.tpe))).checked

  def ByNameTypeTree(result: Tree)(implicit ctx: Context): ByNameTypeTree =
    untpd.ByNameTypeTree(result).withType(ExprType(result.tpe)).checked

  def TypeBoundsTree(lo: Tree, hi: Tree)(implicit ctx: Context): TypeBoundsTree =
    untpd.TypeBoundsTree(lo, hi).withType(TypeBounds(lo.tpe, hi.tpe)).checked

  def Bind(sym: TermSymbol, body: Tree)(implicit ctx: Context): Bind =
    untpd.Bind(sym.name, body).withType(sym.termRef).checked

  def Alternative(trees: List[Tree])(implicit ctx: Context): Alternative =
    untpd.Alternative(trees).withType(ctx.typeComparer.lub(trees map (_.tpe))).checked

  def UnApply(fun: Tree, implicits: List[Tree], patterns: List[Tree])(implicit ctx: Context): UnApply = {
    val owntype = fun.tpe.widen match {
      case MethodType(_, paramType :: Nil) => paramType
      case _ => check(false); ErrorType
    }
    untpd.UnApply(fun, implicits, patterns).withType(owntype).checked
  }

  def ValDef(sym: TermSymbol, rhs: Tree = EmptyTree)(implicit ctx: Context): ValDef =
    untpd.ValDef(Modifiers(sym), sym.name, TypeTree(sym.info), rhs).withType(sym.valRef).checked

  def DefDef(sym: TermSymbol, rhs: Tree = EmptyTree)(implicit ctx: Context): DefDef =
    DefDef(sym, Function.const(rhs) _)

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
    untpd.DefDef(
      Modifiers(sym), sym.name, tparams map TypeDef,
      vparamss map (_ map (ValDef(_))), TypeTree(rtp), rhsFn(argss))
      .withType(sym.termRefWithSig).checked
  }

  def TypeDef(sym: TypeSymbol)(implicit ctx: Context): TypeDef =
    untpd.TypeDef(Modifiers(sym), sym.name, TypeTree(sym.info))
      .withType(sym.typeRef).checked

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
    untpd.TypeDef(Modifiers(cls), cls.name, impl)
      .withType(cls.typeRef).checked
  }

  def Import(expr: Tree, selectors: List[untpd.Tree])(implicit ctx: Context): Import =
    untpd.Import(expr, selectors).withType(ctx.newImportSymbol(SharedTree(expr)).termRef).checked

  def PackageDef(pid: RefTree, stats: List[Tree])(implicit ctx: Context): PackageDef =
    untpd.PackageDef(pid, stats).withType(pid.symbol.namedType).checked

  def Annotated(annot: Tree, arg: Tree)(implicit ctx: Context): Annotated =
    untpd.Annotated(annot, arg).withType(AnnotatedType(Annotation(annot), arg.tpe)).checked

  def SharedTree(tree: Tree)(implicit ctx: Context): SharedTree =
    Trees.SharedTree(tree).withType(tree.tpe)

  // ------ Making references ------------------------------------------------------

  /** A tree representing the same reference as the given type */
  def ref(tp: NamedType)(implicit ctx: Context): NameTree =
    if (tp.symbol.isStatic) Ident(tp)
    else tp.prefix match {
      case pre: TermRef => Select(ref(pre), tp)
      case pre => SelectFromTypeTree(TypeTree(pre), tp)
    } // no checks necessary

  def ref(sym: Symbol)(implicit ctx: Context): tpd.NameTree =
    ref(NamedType(sym.owner.thisType, sym.name, sym.denot))

  // ----- Converting to releated trees -----------------------------------------------

  def underlyingIfRepeated(id: Ident)(implicit ctx: Context): Ident =
    if (id.isType) id else id withType id.tpe.underlyingIfRepeated

  def seqToRepeated(tree: Tree)(implicit ctx: Context): Tree =
    Typed(tree, TypeTree(tree.tpe.widen.translateParameterized(defn.SeqClass, defn.RepeatedParamClass)))

  // ------ Creating typed equivalents of trees that exist only in untyped form -------

  /** new C(args) */
  def New(tp: Type, args: List[Tree])(implicit ctx: Context): Apply = {
    val targs = tp.typeArgs
    Apply(
      Select(
        New(tp withoutArgs targs),
        TermRef(tp.normalizedPrefix, tp.typeSymbol.primaryConstructor.asTerm))
        .appliedToTypes(targs),
      args)
  }

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

    def subst(from: List[Symbol], to: List[Symbol])(implicit ctx: Context): ThisTree =
      new TreeMapper(typeMap = new ctx.SubstSymMap(from, to)).apply(tree)

    def changeOwner(from: Symbol, to: Symbol)(implicit ctx: Context): ThisTree =
      new TreeMapper(ownerMap = (sym => if (sym == from) to else sym)).apply(tree)

    def appliedToTypes(targs: List[Type])(implicit ctx: Context): Tree =
      if (targs.isEmpty) tree else TypeApply(tree, targs map (TypeTree(_)))
  }

  implicit class ListOfTreeDecorator(val xs: List[tpd.Tree]) extends AnyVal {
    def tpes: List[Type] = xs map (_.tpe)
  }

  class TreeMapper(val typeMap: TypeMap = IdentityTypeMap, val ownerMap: Symbol => Symbol = identity _)(implicit ctx: Context) extends TreeTransformer {
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
    for (stat <- stats if stat.isDef) yield stat.symbol
}

