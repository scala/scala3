package dotty.tools.dotc
package core

import Trees._, Positions._, Types._, Contexts._, Constants._, Names._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._

object TypedTrees {

  class TypeTreeGen {
    implicit def pos(implicit ctx: Context): Position = ctx.position

    def defPos(sym: Symbol)(implicit ctx: Context) = ctx.position union sym.coord.toPosition

    def Modifiers(sym: Symbol)(implicit ctx: Context): Modifiers[Type] = Trees.Modifiers[Type](
      sym.flags & ModifierFlags,
      if (sym.privateWithin.exists) sym.privateWithin.asType.name else tpnme.EMPTY,
      sym.annotations map (_.tree))

    def Ident(tp: NamedType)(implicit ctx: Context): Ident[Type] =
      Trees.Ident(tp.name).withType(tp)

    def Select(pre: TypedTree, tp: NamedType)(implicit ctx: Context): Select[Type] =
      Trees.Select(pre, tp.name).withType(tp)

    def This(cls: ClassSymbol)(implicit ctx: Context): This[Type] =
      Trees.This(cls.name).withType(cls.thisType)

    def Super(qual: TypedTree, mixin: Symbol = NoSymbol)(implicit ctx: Context): Super[Type] = {
      val cls = qual.tpe.typeSymbol
      val (owntype, mix) =
        if (mixin.exists) (mixin.typeConstructor, mixin.asType.name)
        else (ctx.glb(cls.info.parents), tpnme.EMPTY)
      Trees.Super(qual, mix).withType(SuperType(qual.tpe, owntype))
    }

    def Apply(fn: TypedTree, args: List[TypedTree])(implicit ctx: Context): Apply[Type] = {
      val fntpe @ MethodType(pnames, ptypes) = fn.tpe
      assert(sameLength(ptypes, args))
      Trees.Apply(fn, args).withType(fntpe.instantiate(args map (_.tpe)))
    }

    def TypeApply(fn: TypedTree, args: List[TypedTree])(implicit ctx: Context): TypeApply[Type] = {
      val fntpe @ PolyType(pnames) = fn.tpe
      assert(sameLength(pnames, args))
      Trees.TypeApply(fn, args).withType(fntpe.instantiate(args map (_.tpe)))
    }

    def Literal(const: Constant)(implicit ctx: Context): Literal[Type] =
      Trees.Literal(const).withType(const.tpe)

    def New(tp: Type)(implicit ctx: Context): New[Type] =
      Trees.New(TypeTree(tp))

    def Pair(left: TypedTree, right: TypedTree)(implicit ctx: Context): Pair[Type] =
      Trees.Pair(left, right).withType(defn.PairType.appliedTo(left.tpe, right.tpe))

    def Typed(expr: TypedTree, tpt: TypedTree)(implicit ctx: Context): Typed[Type] =
      Trees.Typed(expr, tpt).withType(tpt.tpe)

    def NamedArg[Type](name: Name, arg: TypedTree)(implicit ctx: Context) =
      Trees.NamedArg(name, arg).withType(arg.tpe)

    def Assign(lhs: TypedTree, rhs: TypedTree)(implicit ctx: Context): Assign[Type] =
      Trees.Assign(lhs, rhs).withType(defn.UnitType)

    def Function(vparams: List[ValDef[Type]], body: TypedTree)(implicit ctx: Context): Function[Type] =
      Trees.Function(vparams, body)
        .withType(defn.FunctionType(vparams map (_.tpt.tpe), body.tpe))

    def Block(stats: List[TypedTree], expr: TypedTree)(implicit ctx: Context): Block[Type] =
      Trees.Block(stats, expr).withType(expr.tpe) // !!! need to make sure that type does not refer to locals

    def If(cond: TypedTree, thenp: TypedTree, elsep: TypedTree)(implicit ctx: Context): If[Type] =
      Trees.If(cond, thenp, elsep).withType(thenp.tpe | elsep.tpe)

    def Match(selector: TypedTree, cases: List[CaseDef[Type]])(implicit ctx: Context): Match[Type] =
      Trees.Match(selector, cases).withType(ctx.lub(cases map (_.body.tpe)))

    def CaseDef(pat: TypedTree, guard: TypedTree, body: TypedTree)(implicit ctx: Context): CaseDef[Type] =
      Trees.CaseDef(pat, guard, body).withType(body.tpe)

    def Return(expr: TypedTree, from: Ident[Type])(implicit ctx: Context): Return[Type] =
      Trees.Return(expr, from).withType(defn.NothingType)

    def Throw(expr: TypedTree)(implicit ctx: Context): Throw[Type] =
      Trees.Throw(expr).withType(defn.NothingType)

    def ArrayValue(elemtpt: TypedTree, elems: List[TypedTree])(implicit ctx: Context): ArrayValue[Type] =
      Trees.ArrayValue(elemtpt, elems).withType(defn.ArrayType.appliedTo(elemtpt.tpe))

    def TypeTree(tp: Type, original: TypedTree = EmptyTree)(implicit ctx: Context): TypeTree[Type] =
      Trees.TypeTree(original).withType(tp)

    def SingletonTypeTree(ref: TypedTree)(implicit ctx: Context): SingletonTypeTree[Type] =
      Trees.SingletonTypeTree(ref).withType(ref.tpe)

    def SelectFromTypeTree(qualifier: TypedTree, name: TypeName)(implicit ctx: Context): SelectFromTypeTree[Type] =
      Trees.SelectFromTypeTree(qualifier, name).withType(TypeRef(qualifier.tpe, name))

    def AndTypeTree(left: TypedTree, right: TypedTree)(implicit ctx: Context): AndTypeTree[Type] =
      Trees.AndTypeTree(left, right).withType(left.tpe & right.tpe)

    def OrTypeTree(left: TypedTree, right: TypedTree)(implicit ctx: Context): OrTypeTree[Type] =
      Trees.OrTypeTree(left, right).withType(left.tpe | right.tpe)

    def RefineTypeTree(tpt: TypedTree, refinements: List[DefTree[Type]])(implicit ctx: Context): RefineTypeTree[Type] = {
      def refineType(tp: Type, refinement: Symbol): Type =
        RefinedType(tp, refinement.name, refinement.info)
      Trees.RefineTypeTree(tpt, refinements)
        .withType((tpt.tpe /: (refinements map (_.symbol)))(refineType))
    }

    def refineType(tp: Type, refinement: Symbol)(implicit ctx: Context): Type =
      RefinedType(tp, refinement.name, refinement.info)

    def AppliedTypeTree(tpt: TypedTree, args: List[TypedTree])(implicit ctx: Context): AppliedTypeTree[Type] =
      Trees.AppliedTypeTree(tpt, args).withType(tpt.tpe.appliedTo(args map (_.tpe)))

    def TypeBoundsTree(lo: TypedTree, hi: TypedTree)(implicit ctx: Context): TypeBoundsTree[Type] =
      Trees.TypeBoundsTree(lo, hi).withType(TypeBounds(lo.tpe, hi.tpe))

    def Bind(sym: Symbol, body: TypedTree)(implicit ctx: Context): Bind[Type] =
      Trees.Bind(sym.name, body)(defPos(sym)).withType(sym.info)

    def Alternative(trees: List[TypedTree])(implicit ctx: Context): Alternative[Type] =
      Trees.Alternative(trees).withType(ctx.lub(trees map (_.tpe)))

    def UnApply(fun: TypedTree, args: List[TypedTree])(implicit ctx: Context): UnApply[Type] =
      Trees.UnApply(fun, args).withType(fun.tpe match {
        case MethodType(_, paramTypes) => paramTypes.head
      })

    def refType(sym: Symbol)(implicit ctx: Context) = NamedType(sym.owner.thisType, sym)

    def ValDef(sym: TermSymbol, rhs: TypedTree = EmptyTree)(implicit ctx: Context): ValDef[Type] =
      Trees.ValDef(Modifiers(sym), sym.name, TypeTree(sym.info), rhs)(defPos(sym))
        .withType(refType(sym))

    def DefDef(sym: TermSymbol, rhs: TypedTree = EmptyTree)(implicit ctx: Context): DefDef[Type] = {

      val (tparams, mtp) = sym.info match {
        case tp: PolyType =>
          val paramBounds = (tp.paramNames zip tp.paramBounds).toMap
          def typeParam(name: TypeName): TypeSymbol =
            ctx.newLazySymbol(sym, name, TypeParam, { denot =>
              denot.info = new InstPolyMap(tp, tparamRefs) apply
                paramBounds(denot.symbol.asType.name)
            })
          lazy val tparams = tp.paramNames map typeParam
          lazy val tparamRefs = tparams map (_.typeConstructor)
         (tparams, tp.instantiate(tparamRefs))
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

    def TypeDef(sym: TypeSymbol)(implicit ctx: Context): TypeDef[Type] =
      Trees.TypeDef(Modifiers(sym), sym.name, TypeTree(sym.info))(defPos(sym))
        .withType(refType(sym))

    def ClassDef(cls: ClassSymbol, typeParams: List[TypeSymbol], body: List[TypedTree])(implicit ctx: Context): ClassDef[Type] = {
      val parents = cls.info.parents map (TypeTree(_))
      val selfType =
        if (cls.selfType eq cls.typeConstructor) EmptyValDef
        else ValDef(ctx.newSymbol(cls, nme.THIS, SyntheticArtifact, cls.selfType))
      def isOwnTypeParamAccessor(stat: TypedTree) =
        stat.symbol.owner == cls && (stat.symbol is TypeParam)
      val (tparamAccessors, rest) = body partition isOwnTypeParamAccessor
      val tparams =
        (typeParams map TypeDef) ++
        (tparamAccessors collect {
          case td: TypeDef[_] if !(typeParams contains td.symbol) => td
        })
      val findLocalDummy = new FindLocalDummyAccumulator(cls)
      val localDummy = ((NoSymbol: Symbol) /: body)(findLocalDummy)
        .orElse(ctx.newLocalDummy(cls))
      val impl = Trees.Template(parents, selfType, rest)
        .withType(refType(localDummy))
      Trees.ClassDef(Modifiers(cls), cls.name, tparams, impl)(defPos(cls))
        .withType(refType(cls))
    }

    def Import(expr: TypedTree, selectors: List[UntypedTree])(implicit ctx: Context): Import[Type] =
      Trees.Import(expr, selectors).withType(refType(ctx.newImportSymbol(expr)))

    def PackageDef(pid: RefTree[Type], stats: List[TypedTree])(implicit ctx: Context): PackageDef[Type] =
      Trees.PackageDef(pid, stats).withType(refType(pid.symbol))

    def Annotated(annot: TypedTree, arg: TypedTree)(implicit ctx: Context): Annotated[Type] =
      Trees.Annotated(annot, arg).withType(AnnotatedType(List(Annotation(annot)), arg.tpe))

    def EmptyTree: TypedTree = Trees.EmptyTree[Type]

    def EmptyValDef: ValDef[Type] = Trees.EmptyValDef[Type]

    // ----------------------------------------------------------

    def New(tp: Type, args: List[TypedTree])(implicit ctx: Context): Apply[Type] =
      Apply(
        Select(
          New(tp),
          TermRef(tp.normalizedPrefix, tp.typeSymbol.primaryConstructor.asTerm)),
        args)
  }

  object tpd extends TypeTreeGen

  class FindLocalDummyAccumulator(cls: ClassSymbol)(implicit ctx: Context) extends TreeAccumulator[Symbol, Type] {
    def apply(sym: Symbol, tree: TypedTree) =
      if (sym.exists) sym
      else if (tree.isDef) {
        val owner = tree.symbol.owner
        if (owner.isLocalDummy && owner.owner == cls) owner
        else if (owner == cls) foldOver(sym, tree)
        else sym
      } else foldOver(sym, tree)
  }
}

