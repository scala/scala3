package dotty.tools.dotc
package core

import Positions._, Types._, Contexts._, Constants._, Names._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._

object TypedTrees {

  type Modifiers = Trees.Modifiers[Type]
  type Tree = Trees.Tree[Type]
  type TypTree = Trees.TypTree[Type]
  type TermTree = Trees.TermTree[Type]
  type SymTree = Trees.SymTree[Type]
  type ProxyTree = Trees.ProxyTree[Type]
  type NameTree = Trees.NameTree[Type]
  type RefTree = Trees.RefTree[Type]
  type DefTree = Trees.DefTree[Type]

  type TreeCopier = Trees.TreeCopier[Type]
  type TreeAccumulator[T] = Trees.TreeAccumulator[T, Type]
  type TreeTransformer[C] = Trees.TreeTransformer[Type, C]

  type Ident = Trees.Ident[Type]
  type Select = Trees.Select[Type]
  type This = Trees.This[Type]
  type Super = Trees.Super[Type]
  type Apply = Trees.Apply[Type]
  type TypeApply = Trees.TypeApply[Type]
  type Literal = Trees.Literal[Type]
  type New = Trees.New[Type]
  type Pair = Trees.Pair[Type]
  type Typed = Trees.Typed[Type]
  type NamedArg = Trees.NamedArg[Type]
  type Assign = Trees.Assign[Type]
  type Function = Trees.Function[Type]
  type Block = Trees.Block[Type]
  type If = Trees.If[Type]
  type Match = Trees.Match[Type]
  type CaseDef = Trees.CaseDef[Type]
  type Return = Trees.Return[Type]
  type Try = Trees.Try[Type]
  type Throw = Trees.Throw[Type]
  type ArrayValue = Trees.ArrayValue[Type]
  type TypeTree = Trees.TypeTree[Type]
  type SingletonTypeTree = Trees.SingletonTypeTree[Type]
  type SelectFromTypeTree = Trees.SelectFromTypeTree[Type]
  type AndTypeTree = Trees.AndTypeTree[Type]
  type OrTypeTree = Trees.OrTypeTree[Type]
  type RefineTypeTree = Trees.RefineTypeTree[Type]
  type AppliedTypeTree = Trees.AppliedTypeTree[Type]
  type TypeBoundsTree = Trees.TypeBoundsTree[Type]
  type Bind = Trees.Bind[Type]
  type Alternative = Trees.Alternative[Type]
  type UnApply = Trees.UnApply[Type]
  type ValDef = Trees.ValDef[Type]
  type DefDef = Trees.DefDef[Type]
  type TypeDef = Trees.TypeDef[Type]
  type Template = Trees.Template[Type]
  type ClassDef = Trees.ClassDef[Type]
  type Import = Trees.Import[Type]
  type PackageDef = Trees.PackageDef[Type]
  type Annotated = Trees.Annotated[Type]
  type EmptyTree = Trees.EmptyTree[Type]
  type Shared = Trees.Shared[Type]

  private implicit def pos(implicit ctx: Context): Position = ctx.position

  def defPos(sym: Symbol)(implicit ctx: Context) = ctx.position union sym.coord.toPosition

  def Modifiers(sym: Symbol)(implicit ctx: Context): Modifiers = Trees.Modifiers[Type](
    sym.flags & ModifierFlags,
    if (sym.privateWithin.exists) sym.privateWithin.asType.name else tpnme.EMPTY,
    sym.annotations map (_.tree))

  val Ident = Trees.Ident
  def Ident(tp: NamedType)(implicit ctx: Context): Ident =
    Trees.Ident(tp.name).withType(tp)

  def Select(pre: Tree, tp: NamedType)(implicit ctx: Context): Select =
    Trees.Select(pre, tp.name).withType(tp)

  def This(cls: ClassSymbol)(implicit ctx: Context): This =
    Trees.This(cls.name).withType(cls.thisType)

  def Super(qual: Tree, mixin: Symbol = NoSymbol)(implicit ctx: Context): Super = {
    val cls = qual.tpe.typeSymbol
    val (owntype, mix) =
      if (mixin.exists) (mixin.typeConstructor, mixin.asType.name)
      else (ctx.glb(cls.info.parents), tpnme.EMPTY)
    Trees.Super(qual, mix).withType(SuperType(qual.tpe, owntype))
  }

  def Apply(fn: Tree, args: List[Tree])(implicit ctx: Context): Apply = {
    val fntpe @ MethodType(pnames, ptypes) = fn.tpe
    assert(sameLength(ptypes, args))
    Trees.Apply(fn, args).withType(fntpe.instantiate(args map (_.tpe)))
  }

  def TypeApply(fn: Tree, args: List[Tree])(implicit ctx: Context): TypeApply = {
    val fntpe @ PolyType(pnames) = fn.tpe
    assert(sameLength(pnames, args))
    Trees.TypeApply(fn, args).withType(fntpe.instantiate(args map (_.tpe)))
  }

  def Literal(const: Constant)(implicit ctx: Context): Literal =
    Trees.Literal(const).withType(const.tpe)

  def New(tp: Type)(implicit ctx: Context): New =
    Trees.New(TypeTree(tp))

  def Pair(left: Tree, right: Tree)(implicit ctx: Context): Pair =
    Trees.Pair(left, right).withType(defn.PairType.appliedTo(left.tpe, right.tpe))

  def Typed(expr: Tree, tpt: Tree)(implicit ctx: Context): Typed =
    Trees.Typed(expr, tpt).withType(tpt.tpe)

  def NamedArg(name: Name, arg: Tree)(implicit ctx: Context) =
    Trees.NamedArg(name, arg).withType(arg.tpe)

  def Assign(lhs: Tree, rhs: Tree)(implicit ctx: Context): Assign =
    Trees.Assign(lhs, rhs).withType(defn.UnitType)

  def Function(vparams: List[ValDef], body: Tree)(implicit ctx: Context): Function =
    Trees.Function(vparams, body)
      .withType(defn.FunctionType(vparams map (_.tpt.tpe), body.tpe))

  def Block(stats: List[Tree], expr: Tree)(implicit ctx: Context): Block =
    Trees.Block(stats, expr).withType(expr.tpe) // !!! need to make sure that type does not refer to locals

  def If(cond: Tree, thenp: Tree, elsep: Tree)(implicit ctx: Context): If =
    Trees.If(cond, thenp, elsep).withType(thenp.tpe | elsep.tpe)

  def Match(selector: Tree, cases: List[CaseDef])(implicit ctx: Context): Match =
    Trees.Match(selector, cases).withType(ctx.lub(cases map (_.body.tpe)))

  def CaseDef(pat: Tree, guard: Tree, body: Tree)(implicit ctx: Context): CaseDef =
    Trees.CaseDef(pat, guard, body).withType(body.tpe)

  def Return(expr: Tree, from: Ident)(implicit ctx: Context): Return =
    Trees.Return(expr, from).withType(defn.NothingType)

  def Throw(expr: Tree)(implicit ctx: Context): Throw =
    Trees.Throw(expr).withType(defn.NothingType)

  def ArrayValue(elemtpt: Tree, elems: List[Tree])(implicit ctx: Context): ArrayValue =
    Trees.ArrayValue(elemtpt, elems).withType(defn.ArrayType.appliedTo(elemtpt.tpe))

  def TypeTree(tp: Type, original: Tree = EmptyTree)(implicit ctx: Context): TypeTree =
    Trees.TypeTree(original).withType(tp)

  def SingletonTypeTree(ref: Tree)(implicit ctx: Context): SingletonTypeTree =
    Trees.SingletonTypeTree(ref).withType(ref.tpe)

  def SelectFromTypeTree(qualifier: Tree, name: TypeName)(implicit ctx: Context): SelectFromTypeTree =
    Trees.SelectFromTypeTree(qualifier, name).withType(TypeRef(qualifier.tpe, name))

  def AndTypeTree(left: Tree, right: Tree)(implicit ctx: Context): AndTypeTree =
    Trees.AndTypeTree(left, right).withType(left.tpe & right.tpe)

  def OrTypeTree(left: Tree, right: Tree)(implicit ctx: Context): OrTypeTree =
    Trees.OrTypeTree(left, right).withType(left.tpe | right.tpe)

  def RefineTypeTree(tpt: Tree, refinements: List[DefTree])(implicit ctx: Context): RefineTypeTree = {
    def refineType(tp: Type, refinement: Symbol): Type =
      RefinedType(tp, refinement.name, refinement.info)
    Trees.RefineTypeTree(tpt, refinements)
      .withType((tpt.tpe /: (refinements map (_.symbol)))(refineType))
  }

  def refineType(tp: Type, refinement: Symbol)(implicit ctx: Context): Type =
    RefinedType(tp, refinement.name, refinement.info)

  def AppliedTypeTree(tpt: Tree, args: List[Tree])(implicit ctx: Context): AppliedTypeTree =
    Trees.AppliedTypeTree(tpt, args).withType(tpt.tpe.appliedTo(args map (_.tpe)))

  def TypeBoundsTree(lo: Tree, hi: Tree)(implicit ctx: Context): TypeBoundsTree =
    Trees.TypeBoundsTree(lo, hi).withType(TypeBounds(lo.tpe, hi.tpe))

  def Bind(sym: Symbol, body: Tree)(implicit ctx: Context): Bind =
    Trees.Bind(sym.name, body)(defPos(sym)).withType(sym.info)

  def Alternative(trees: List[Tree])(implicit ctx: Context): Alternative =
    Trees.Alternative(trees).withType(ctx.lub(trees map (_.tpe)))

  def UnApply(fun: Tree, args: List[Tree])(implicit ctx: Context): UnApply =
    Trees.UnApply(fun, args).withType(fun.tpe match {
      case MethodType(_, paramTypes) => paramTypes.head
    })

  def refType(sym: Symbol)(implicit ctx: Context) = NamedType(sym.owner.thisType, sym)

  def ValDef(sym: TermSymbol, rhs: Tree = EmptyTree)(implicit ctx: Context): ValDef =
    Trees.ValDef(Modifiers(sym), sym.name, TypeTree(sym.info), rhs)(defPos(sym))
      .withType(refType(sym))

  def DefDef(sym: TermSymbol, rhs: Tree = EmptyTree)(implicit ctx: Context): DefDef = {

    val (tparams, mtp) = sym.info match {
      case tp: PolyType =>
        def paramBounds(trefs: List[TypeRef]): List[Type] =
          tp.paramBounds map new InstPolyMap(tp, trefs)
        val tparams = ctx.newTypeParams(sym, tp.paramNames, EmptyFlags, paramBounds)
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

    Trees.DefDef(
      Modifiers(sym), sym.name, tparams map TypeDef,
      vparamss map (_ map (ValDef(_))), TypeTree(rtp), rhs)(defPos(sym))
      .withType(refType(sym))
  }

  def TypeDef(sym: TypeSymbol)(implicit ctx: Context): TypeDef =
    Trees.TypeDef(Modifiers(sym), sym.name, TypeTree(sym.info))(defPos(sym))
      .withType(refType(sym))

  def ClassDef(cls: ClassSymbol, typeParams: List[TypeSymbol], body: List[Tree])(implicit ctx: Context): ClassDef = {
    val parents = cls.info.parents map (TypeTree(_))
    val selfType =
      if (cls.selfType eq cls.typeConstructor) EmptyValDef
      else ValDef(ctx.newSelfSym(cls))
    def isOwnTypeParamAccessor(stat: Tree) =
      stat.symbol.owner == cls && (stat.symbol is TypeParam)
    val (tparamAccessors, rest) = body partition isOwnTypeParamAccessor
    val tparams =
      (typeParams map TypeDef) ++
        (tparamAccessors collect {
          case td: TypeDef if !(typeParams contains td.symbol) => td
        })
    val findLocalDummy = new FindLocalDummyAccumulator(cls)
    val localDummy = ((NoSymbol: Symbol) /: body)(findLocalDummy)
      .orElse(ctx.newLocalDummy(cls))
    val impl = Trees.Template(parents, selfType, rest)
      .withType(refType(localDummy))
    Trees.ClassDef(Modifiers(cls), cls.name, tparams, impl)(defPos(cls))
      .withType(refType(cls))
  }

  def Import(expr: Tree, selectors: List[Trees.UntypedTree])(implicit ctx: Context): Import =
    Trees.Import(expr, selectors).withType(refType(ctx.newImportSymbol(expr)))

  def PackageDef(pid: RefTree, stats: List[Tree])(implicit ctx: Context): PackageDef =
    Trees.PackageDef(pid, stats).withType(refType(pid.symbol))

  def Annotated(annot: Tree, arg: Tree)(implicit ctx: Context): Annotated =
    Trees.Annotated(annot, arg).withType(AnnotatedType(List(Annotation(annot)), arg.tpe))

  val EmptyTree: Tree = Trees.EmptyTree[Type]

  val EmptyValDef: ValDef = Trees.EmptyValDef[Type]

  def Shared(tree: Tree): Shared =
    Trees.Shared(tree).withType(tree.tpe)

  // -----------------------------------------------------------------------------

  def New(tp: Type, args: List[Tree])(implicit ctx: Context): Apply =
    Apply(
      Select(
        New(tp),
        TermRef(tp.normalizedPrefix, tp.typeSymbol.primaryConstructor.asTerm)),
      args)

  def ModuleDef(sym: TermSymbol, body: List[Tree])(implicit ctx: Context): ValDef = {
    val modcls = sym.moduleClass.asClass
    val clsdef = ClassDef(modcls, Nil, body)
    val rhs = Block(List(clsdef), New(modcls.typeConstructor))
    ValDef(sym, rhs)
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
}

