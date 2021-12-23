package dotty.tools
package dotc
package cc

import core._
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*
import config.Printers.capt
import ast.tpd
import transform.Recheck.*

class Setup(
  preRecheckPhase: DenotTransformer,
  thisPhase: DenotTransformer,
  recheckDef: (tpd.ValOrDefDef, Symbol) => Context ?=> Unit)
extends tpd.TreeTraverser:
  import tpd.*

  private def depFun(tycon: Type, argTypes: List[Type], resType: Type)(using Context): Type =
    MethodType.companion(
        isContextual = defn.isContextFunctionClass(tycon.classSymbol),
        isErased = defn.isErasedFunctionClass(tycon.classSymbol)
      )(argTypes, resType)
      .toFunctionType(isJava = false, alwaysDependent = true)

  private def box(tp: Type)(using Context): Type = tp match
    case CapturingType(parent, refs, false) => CapturingType(parent, refs, true)
    case _ => tp

  private def setBoxed(tp: Type)(using Context) = tp match
    case AnnotatedType(_, annot) if annot.symbol == defn.RetainsAnnot =>
      annot.tree.setBoxedCapturing()
    case _ =>

  private def addBoxes(using Context) = new TypeTraverser:
    def traverse(t: Type) =
      t match
        case AppliedType(tycon, args) if !defn.isNonRefinedFunction(t) =>
          args.foreach(setBoxed)
        case TypeBounds(lo, hi) =>
          setBoxed(lo); setBoxed(hi)
        case _ =>
      traverseChildren(t)

  /** Perform the following transformation steps everywhere in a type:
    *  1. Drop retains annotations
    *  2. Turn plain function types into dependent function types, so that
    *     we can refer to their parameter in capture sets. Currently this is
    *     only done at the toplevel, i.e. for function types that are not
    *     themselves argument types of other function types. Without this restriction
    *     boxmap-paper.scala fails. Need to figure out why.
    *  3. Refine other class types C by adding capture set variables to their parameter getters
    *     (see addCaptureRefinements)
    *  4. Add capture set variables to all types that can be tracked
    *
    *  Polytype bounds are only cleaned using step 1, but not otherwise transformed.
    */
  private def mapInferred(using Context) = new TypeMap:

    /** Drop @retains annotations everywhere */
    object cleanup extends TypeMap:
      def apply(t: Type) = t match
        case AnnotatedType(parent, annot) if annot.symbol == defn.RetainsAnnot =>
          apply(parent)
        case _ =>
          mapOver(t)

    /** Refine a possibly applied class type C where the class has tracked parameters
     *  x_1: T_1, ..., x_n: T_n to C { val x_1: CV_1 T_1, ..., val x_n: CV_n T_n }
     *  where CV_1, ..., CV_n are fresh capture sets.
     */
    def addCaptureRefinements(tp: Type): Type = tp match
      case _: TypeRef | _: AppliedType if tp.typeParams.isEmpty =>
        tp.typeSymbol match
          case cls: ClassSymbol if !defn.isFunctionClass(cls) =>
            cls.paramGetters.foldLeft(tp) { (core, getter) =>
              if getter.termRef.isTracked then
                val getterType = tp.memberInfo(getter).strippedDealias
                RefinedType(core, getter.name, CapturingType(getterType, CaptureSet.Var(), boxed = false))
                  .showing(i"add capture refinement $tp --> $result", capt)
              else
                core
            }
          case _ => tp
      case _ => tp

    /** Should a capture set variable be added on type `tp`? */
    def canHaveInferredCapture(tp: Type): Boolean =
      tp.typeParams.isEmpty && tp.match
        case tp: (TypeRef | AppliedType) =>
          val sym = tp.typeSymbol
          if sym.isClass then !sym.isValueClass && sym != defn.AnyClass
          else canHaveInferredCapture(tp.superType.dealias)
        case tp: (RefinedOrRecType | MatchType) =>
          canHaveInferredCapture(tp.underlying)
        case tp: AndType =>
          canHaveInferredCapture(tp.tp1) && canHaveInferredCapture(tp.tp2)
        case tp: OrType =>
          canHaveInferredCapture(tp.tp1) || canHaveInferredCapture(tp.tp2)
        case _ =>
          false

    /** Add a capture set variable to `tp` if necessary, or maybe pull out
     *  an embedded capture set variables from a part of `tp`.
     */
    def addVar(tp: Type) = tp match
      case tp @ RefinedType(parent @ CapturingType(parent1, refs, boxed), rname, rinfo) =>
        CapturingType(tp.derivedRefinedType(parent1, rname, rinfo), refs, boxed)
      case tp: RecType =>
        tp.parent match
          case CapturingType(parent1, refs, boxed) =>
            CapturingType(tp.derivedRecType(parent1), refs, boxed)
          case _ =>
            tp // can return `tp` here since unlike RefinedTypes, RecTypes are never created
                // by `mapInferred`. Hence if the underlying type admits capture variables
                // a variable was already added, and the first case above would apply.
      case AndType(CapturingType(parent1, refs1, boxed1), CapturingType(parent2, refs2, boxed2)) =>
        assert(refs1.asVar.elems.isEmpty)
        assert(refs2.asVar.elems.isEmpty)
        assert(boxed1 == boxed2)
        CapturingType(AndType(parent1, parent2), refs1, boxed1)
      case tp @ OrType(CapturingType(parent1, refs1, boxed1), CapturingType(parent2, refs2, boxed2)) =>
        assert(refs1.asVar.elems.isEmpty)
        assert(refs2.asVar.elems.isEmpty)
        assert(boxed1 == boxed2)
        CapturingType(OrType(parent1, parent2, tp.isSoft), refs1, boxed1)
      case tp @ OrType(CapturingType(parent1, refs1, boxed1), tp2) =>
        CapturingType(OrType(parent1, tp2, tp.isSoft), refs1, boxed1)
      case tp @ OrType(tp1, CapturingType(parent2, refs2, boxed2)) =>
        CapturingType(OrType(tp1, parent2, tp.isSoft), refs2, boxed2)
      case _ if canHaveInferredCapture(tp) =>
        CapturingType(tp, CaptureSet.Var(), boxed = false)
      case _ =>
        tp

    var isTopLevel = true

    def mapNested(ts: List[Type]): List[Type] =
      val saved = isTopLevel
      isTopLevel = false
      try ts.mapConserve(this) finally isTopLevel = saved

    def apply(t: Type) =
      val t1 = t match
        case AnnotatedType(parent, annot) if annot.symbol == defn.RetainsAnnot =>
          apply(parent)
        case tp @ AppliedType(tycon, args) =>
          val tycon1 = this(tycon)
          if defn.isNonRefinedFunction(tp) then
            val args1 = mapNested(args.init)
            val res1 = this(args.last)
            if isTopLevel then
              depFun(tycon1, args1, res1)
                .showing(i"add function refinement $tp --> $result", capt)
            else
              tp.derivedAppliedType(tycon1, args1 :+ res1)
          else
            tp.derivedAppliedType(tycon1, args.mapConserve(arg => box(this(arg))))
        case tp @ RefinedType(core, rname, rinfo) if defn.isFunctionType(tp) =>
          val rinfo1 = apply(rinfo)
          if rinfo1 ne rinfo then rinfo1.toFunctionType(isJava = false, alwaysDependent = true)
          else tp
        case tp: MethodType =>
          tp.derivedLambdaType(
            paramInfos = mapNested(tp.paramInfos),
            resType = this(tp.resType))
        case tp: TypeLambda =>
          // Don't recurse into parameter bounds, just cleanup any stray retains annotations
          tp.derivedLambdaType(
            paramInfos = tp.paramInfos.mapConserve(cleanup(_).bounds),
            resType = this(tp.resType))
        case _ =>
          mapOver(t)
      addVar(addCaptureRefinements(t1))
  end mapInferred

  private def expandAbbreviations(using Context) = new TypeMap:

    def propagateMethodResult(tp: Type, outerCs: CaptureSet, deep: Boolean): Type = tp match
      case tp: MethodType =>
        if deep then
          val tp1 = tp.derivedLambdaType(paramInfos = tp.paramInfos.mapConserve(this))
          propagateMethodResult(tp1, outerCs, deep = false)
        else
          val localCs = CaptureSet(tp.paramRefs.filter(_.isTracked)*)
          tp.derivedLambdaType(
            resType = propagateEnclosing(tp.resType, CaptureSet.empty, outerCs ++ localCs))

    def propagateDepFunctionResult(tp: Type, outerCs: CaptureSet, deep: Boolean): Type = tp match
      case tp @ RefinedType(parent, nme.apply, rinfo: MethodType) =>
        val rinfo1 = propagateMethodResult(rinfo, outerCs, deep)
        if rinfo1 ne rinfo then rinfo1.toFunctionType(isJava = false, alwaysDependent = true)
        else tp

    def propagateEnclosing(tp: Type, currentCs: CaptureSet, outerCs: CaptureSet): Type = tp match
      case tp @ AppliedType(tycon, args) if defn.isFunctionClass(tycon.typeSymbol) =>
        val tycon1 = this(tycon)
        val args1 = args.init.mapConserve(this)
        val tp1 =
          if args1.exists(!_.captureSet.isAlwaysEmpty) then
            val propagated = propagateDepFunctionResult(
              depFun(tycon, args1, args.last), currentCs ++ outerCs, deep = false)
            propagated match
              case RefinedType(_, _, mt: MethodType) =>
                val following = mt.resType.captureSet.elems
                if mt.paramRefs.exists(following.contains(_)) then propagated
                else tp.derivedAppliedType(tycon1, args1 :+ mt.resType)
          else
            val resType1 = propagateEnclosing(
              args.last, CaptureSet.empty, currentCs ++ outerCs)
            tp.derivedAppliedType(tycon1, args1 :+ resType1)
        tp1.capturing(outerCs)
      case tp @ RefinedType(parent, nme.apply, rinfo: MethodType) if defn.isFunctionType(tp) =>
        propagateDepFunctionResult(tp, currentCs ++ outerCs, deep = true)
          .capturing(outerCs)
      case _ =>
        mapOver(tp)

    def apply(tp: Type): Type = tp match
      case CapturingType(parent, cs, boxed) =>
        tp.derivedCapturingType(propagateEnclosing(parent, cs, CaptureSet.empty), cs)
      case _ =>
        propagateEnclosing(tp, CaptureSet.empty, CaptureSet.empty)
  end expandAbbreviations

  private def transformInferredType(tp: Type, boxed: Boolean)(using Context): Type =
    val tp1 = mapInferred(tp)
    if boxed then box(tp1) else tp1

  private def transformExplicitType(tp: Type, boxed: Boolean)(using Context): Type =
    addBoxes.traverse(tp)
    if boxed then setBoxed(tp)
    if ctx.settings.YccNoAbbrev.value then tp
    else expandAbbreviations(tp)

  // Substitute parameter symbols in `from` to paramRefs in corresponding
  // method or poly types `to`. We use a single BiTypeMap to do everything.
  private class SubstParams(from: List[List[Symbol]], to: List[LambdaType])(using Context)
  extends DeepTypeMap, BiTypeMap:

    def apply(t: Type): Type = t match
      case t: NamedType =>
        val sym = t.symbol
        def outer(froms: List[List[Symbol]], tos: List[LambdaType]): Type =
          def inner(from: List[Symbol], to: List[ParamRef]): Type =
            if from.isEmpty then outer(froms.tail, tos.tail)
            else if sym eq from.head then to.head
            else inner(from.tail, to.tail)
          if tos.isEmpty then t
          else inner(froms.head, tos.head.paramRefs)
        outer(from, to)
      case _ =>
        mapOver(t)

    def inverse(t: Type): Type = t match
      case t: ParamRef =>
        def recur(from: List[LambdaType], to: List[List[Symbol]]): Type =
          if from.isEmpty then t
          else if t.binder eq from.head then to.head(t.paramNum).namedType
          else recur(from.tail, to.tail)
        recur(to, from)
      case _ =>
        mapOver(t)
  end SubstParams

  private def transformTT(tree: TypeTree, boxed: Boolean)(using Context) =
    tree.rememberType(
      if tree.isInstanceOf[InferredTypeTree]
      then transformInferredType(tree.tpe, boxed)
      else transformExplicitType(tree.tpe, boxed))

  def traverse(tree: Tree)(using Context) =
    tree match
      case tree @ ValDef(_, tpt: TypeTree, _) if tree.symbol.is(Mutable) =>
        transformTT(tpt, boxed = true)
        traverse(tree.rhs)
      case _ =>
        traverseChildren(tree)
    tree match
      case tree: TypeTree =>
        transformTT(tree, boxed = false)
      case tree: ValOrDefDef =>
        val sym = tree.symbol

        // replace an existing symbol info with inferred types
        def integrateRT(
            info: Type,                     // symbol info to replace
            psymss: List[List[Symbol]],     // the local (type and trem) parameter symbols corresponding to `info`
            prevPsymss: List[List[Symbol]], // the local parameter symbols seen previously in reverse order
            prevLambdas: List[LambdaType]   // the outer method and polytypes generated previously in reverse order
          ): Type =
          info match
            case mt: MethodOrPoly =>
              val psyms = psymss.head
              mt.companion(mt.paramNames)(
                mt1 =>
                  if !psyms.exists(_.isUpdatedAfter(preRecheckPhase)) && !mt.isParamDependent && prevLambdas.isEmpty then
                    mt.paramInfos
                  else
                    val subst = SubstParams(psyms :: prevPsymss, mt1 :: prevLambdas)
                    psyms.map(psym => subst(psym.info).asInstanceOf[mt.PInfo]),
                mt1 =>
                  integrateRT(mt.resType, psymss.tail, psyms :: prevPsymss, mt1 :: prevLambdas)
              )
            case info: ExprType =>
              info.derivedExprType(resType =
                integrateRT(info.resType, psymss, prevPsymss, prevLambdas))
            case _ =>
              val restp = tree.tpt.knownType
              if prevLambdas.isEmpty then restp
              else SubstParams(prevPsymss, prevLambdas)(restp)

        if tree.tpt.hasRememberedType && !sym.isConstructor then
          val newInfo = integrateRT(sym.info, sym.paramSymss, Nil, Nil)
            .showing(i"update info $sym: ${sym.info} --> $result", capt)
          if newInfo ne sym.info then
            val completer = new LazyType:
              def complete(denot: SymDenotation)(using Context) =
                denot.info = newInfo
                recheckDef(tree, sym)
            sym.updateInfoBetween(preRecheckPhase, thisPhase, completer)
      case tree: Bind =>
        val sym = tree.symbol
        sym.updateInfoBetween(preRecheckPhase, thisPhase,
          transformInferredType(sym.info, boxed = false))
      case _ =>
end Setup
