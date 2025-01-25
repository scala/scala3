package dotty.tools
package dotc
package cc
import ast.tpd
import collection.mutable

import core.*
import Symbols.*, Types.*, Flags.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import CaptureSet.{Refs, emptySet, HiddenSet}
import config.Printers.capt
import StdNames.nme
import util.{SimpleIdentitySet, EqHashMap, SrcPos}

object SepChecker:

  /** Enumerates kinds of captures encountered so far */
  enum Captures:
    case None
    case Explicit   // one or more explicitly declared captures
    case Hidden     // exacttly one hidden captures
    case NeedsCheck // one hidden capture and one other capture (hidden or declared)

    def add(that: Captures): Captures =
      if this == None then that
      else if that == None then this
      else if this == Explicit && that == Explicit then Explicit
      else NeedsCheck
  end Captures

  /** The kind of checked type, used for composing error messages */
  enum TypeKind:
    case Result(sym: Symbol, inferred: Boolean)
    case Argument

    def dclSym = this match
      case Result(sym, _) => sym
      case _ => NoSymbol
  end TypeKind

class SepChecker(checker: CheckCaptures.CheckerAPI) extends tpd.TreeTraverser:
  import tpd.*
  import checker.*
  import SepChecker.*

  /** The set of capabilities that are hidden by a polymorphic result type
   *  of some previous definition.
   */
  private var defsShadow: Refs = SimpleIdentitySet.empty

  /** A map from definitions to their internal result types.
   *  Populated during separation checking traversal.
   */
  private val resultType = EqHashMap[Symbol, Type]()

  /** The previous val or def definitions encountered during separation checking.
   *  These all enclose and precede the current traversal node.
   */
  private var previousDefs: List[mutable.ListBuffer[ValOrDefDef]] = Nil

  extension (refs: Refs)
    private def footprint(using Context): Refs =
      def recur(elems: Refs, newElems: List[CaptureRef]): Refs = newElems match
        case newElem :: newElems1 =>
          val superElems = newElem.captureSetOfInfo.elems.filter: superElem =>
            !superElem.isMaxCapability && !elems.contains(superElem)
          recur(elems ++ superElems, newElems1 ++ superElems.toList)
        case Nil => elems
      val elems: Refs = refs.filter(!_.isMaxCapability)
      recur(elems, elems.toList)

    private def overlapWith(other: Refs)(using Context): Refs =
      val refs1 = refs
      val refs2 = other
      def common(refs1: Refs, refs2: Refs) =
        refs1.filter: ref =>
          ref.isExclusive && refs2.exists(_.stripReadOnly eq ref)
      common(refs, other) ++ common(other, refs)

    private def hidden(using Context): Refs =
      val seen: util.EqHashSet[CaptureRef] = new util.EqHashSet
      def recur(cs: Refs): Refs =
        (emptySet /: cs): (elems, elem) =>
          if seen.add(elem) then elems ++ hiddenByElem(elem, recur)
          else elems
      recur(refs)
    end hidden

    private def containsHidden(using Context): Boolean =
      refs.exists: ref =>
        !hiddenByElem(ref, _ => emptySet).isEmpty

    /** Deduct the footprint of `sym` and `sym*` from `refs` */
    private def deductSym(sym: Symbol)(using Context) =
      val ref = sym.termRef
      if ref.isTrackableRef then refs -- CaptureSet(ref, ref.reach).elems.footprint
      else refs

    /** Deduct the footprint of all captures of `deps` from `refs` */
    private def deductCapturesOf(deps: List[Tree])(using Context): Refs =
      deps.foldLeft(refs): (refs, dep) =>
        refs -- captures(dep).footprint
  end extension

  private def hiddenByElem(ref: CaptureRef, recur: Refs => Refs)(using Context): Refs = ref match
    case Fresh.Cap(hcs) => hcs.elems.filter(!_.isRootCapability) ++ recur(hcs.elems)
    case ReadOnlyCapability(ref1) => hiddenByElem(ref1, recur).map(_.readOnly)
    case _ => emptySet

  /** The captures of an argument or prefix widened to the formal parameter, if
   *  the latter contains a cap.
   */
  private def formalCaptures(arg: Tree)(using Context): Refs =
    arg.formalType.orElse(arg.nuType).deepCaptureSet.elems

   /** The captures of a node */
  private def captures(tree: Tree)(using Context): Refs =
   tree.nuType.deepCaptureSet.elems

  private def sepApplyError(fn: Tree, args: List[Tree], argIdx: Int,
      overlap: Refs, hiddenInArg: Refs, footprints: List[(Refs, Int)],
      deps: collection.Map[Tree, List[Tree]])(using Context): Unit =
    val arg = args(argIdx)
    def paramName(mt: Type, idx: Int): Option[Name] = mt match
      case mt @ MethodType(pnames) =>
        if idx < pnames.length then Some(pnames(idx)) else paramName(mt.resType, idx - pnames.length)
      case mt: PolyType => paramName(mt.resType, idx)
      case _ => None
    def formalName = paramName(fn.nuType.widen, argIdx) match
      case Some(pname) => i"$pname "
      case _ => ""
    def whatStr = if overlap.size == 1 then "this capability is" else "these capabilities are"
    def qualifier = methPart(fn) match
      case Select(qual, _) => qual
      case _ => EmptyTree
    def isShowableMethod = fn.symbol.exists && !defn.isFunctionSymbol(fn.symbol.maybeOwner)
    def funType =
      if fn.symbol.exists && !qualifier.isEmpty then qualifier.nuType else fn.nuType
    def funStr =
      if isShowableMethod then i"${fn.symbol}: ${fn.symbol.info}"
      else i"a function of type ${funType.widen}"
    val clashIdx = footprints
      .collect:
        case (fp, idx) if !hiddenInArg.overlapWith(fp).isEmpty => idx
      .head
    def whereStr = clashIdx match
      case 0 => "function prefix"
      case 1 => "first argument "
      case 2 => "second argument"
      case 3 => "third argument "
      case n => s"${n}th argument  "
    def clashTree =
      if clashIdx == 0 then qualifier
      else args(clashIdx - 1)
    def clashTypeStr =
      if clashIdx == 0 && !isShowableMethod then "" // we already mentioned the type in `funStr`
      else i" with type  ${clashTree.nuType}"
    def clashCaptures = captures(clashTree)
    def hiddenCaptures = formalCaptures(arg).hidden
    def clashFootprint = clashCaptures.footprint
    def hiddenFootprint = hiddenCaptures.footprint
    def declaredFootprint = deps(arg).map(captures(_)).foldLeft(emptySet)(_ ++ _).footprint
    def footprintOverlap = hiddenFootprint.overlapWith(clashFootprint) -- declaredFootprint
    report.error(
      em"""Separation failure: argument of type  ${arg.nuType}
          |to $funStr
          |corresponds to capture-polymorphic formal parameter ${formalName}of type  ${arg.formalType}
          |and captures ${CaptureSet(overlap)}, but $whatStr also passed separately
          |in the ${whereStr.trim}$clashTypeStr.
          |
          |  Capture set of $whereStr        : ${CaptureSet(clashCaptures)}
          |  Hidden set of current argument        : ${CaptureSet(hiddenCaptures)}
          |  Footprint of $whereStr          : ${CaptureSet(clashFootprint)}
          |  Hidden footprint of current argument  : ${CaptureSet(hiddenFootprint)}
          |  Declared footprint of current argument: ${CaptureSet(declaredFootprint)}
          |  Undeclared overlap of footprints      : ${CaptureSet(footprintOverlap)}""",
      arg.srcPos)
  end sepApplyError

  def sepUseError(tree: Tree, used: Refs, globalOverlap: Refs)(using Context): Unit =
    val individualChecks = for mdefs <- previousDefs.iterator; mdef <- mdefs.iterator yield
      val hiddenByDef = captures(mdef.tpt).hidden.footprint
      val overlap = defUseOverlap(hiddenByDef, used, tree.symbol)
      if !overlap.isEmpty then
        def resultStr = if mdef.isInstanceOf[DefDef] then " result" else ""
        report.error(
          em"""Separation failure: Illegal access to ${CaptureSet(overlap)} which is hidden by the previous definition
              |of ${mdef.symbol} with$resultStr type ${mdef.tpt.nuType}.
              |This type hides capabilities  ${CaptureSet(hiddenByDef)}""",
          tree.srcPos)
        true
      else false
    val clashes = individualChecks.filter(identity)
    if clashes.hasNext then clashes.next // issues error as a side effect
    else report.error(
      em"""Separation failure: Illegal access to ${CaptureSet(globalOverlap)} which is hidden by some previous definitions
          |No clashing definitions were found. This might point to an internal error.""",
      tree.srcPos)
  end sepUseError

  private def checkApply(fn: Tree, args: List[Tree], deps: collection.Map[Tree, List[Tree]])(using Context): Unit =
    val fnCaptures = methPart(fn) match
      case Select(qual, _) => qual.nuType.captureSet
      case _ => CaptureSet.empty
    capt.println(i"check separate $fn($args), fnCaptures = $fnCaptures, argCaptures = ${args.map(arg => CaptureSet(formalCaptures(arg)))}, deps = ${deps.toList}")
    var footprint = fnCaptures.elems.footprint
    val footprints = mutable.ListBuffer[(Refs, Int)]((footprint, 0))
    val indexedArgs = args.zipWithIndex

    for (arg, idx) <- indexedArgs do
      if !arg.needsSepCheck then
        footprint = footprint ++ captures(arg).footprint.deductCapturesOf(deps(arg))
        footprints += ((footprint, idx + 1))
    for (arg, idx) <- indexedArgs do
      if arg.needsSepCheck then
        val ac = formalCaptures(arg)
        checkType(arg.formalType, arg.srcPos, TypeKind.Argument)
        val hiddenInArg = ac.hidden.footprint
        //println(i"check sep $arg: $ac, footprint so far = $footprint, hidden = $hiddenInArg")
        val overlap = hiddenInArg.overlapWith(footprint).deductCapturesOf(deps(arg))
        if !overlap.isEmpty then
          sepApplyError(fn, args, idx, overlap, hiddenInArg, footprints.toList, deps)
        footprint ++= captures(arg).footprint
        footprints += ((footprint, idx + 1))
  end checkApply

  def defUseOverlap(hiddenByDef: Refs, used: Refs, sym: Symbol)(using Context): Refs =
    val overlap = hiddenByDef.overlapWith(used)
    resultType.get(sym) match
      case Some(tp) if !overlap.isEmpty =>
        val declared = tp.captureSet.elems
        overlap -- declared.footprint -- declared.hidden.footprint
      case _ =>
        overlap

  def checkUse(tree: Tree)(using Context) =
    val used = tree.markedFree
    if !used.elems.isEmpty then
      val usedFootprint = used.elems.footprint
      val overlap = defUseOverlap(defsShadow, usedFootprint, tree.symbol)
      if !overlap.isEmpty then
        sepUseError(tree, usedFootprint, overlap)

  def checkType(tpt: Tree, sym: Symbol)(using Context): Unit =
    checkType(tpt.nuType, tpt.srcPos,
        TypeKind.Result(sym, inferred = tpt.isInstanceOf[InferredTypeTree]))

  /** Check that all parts of type `tpe` are separated. */
  def checkType(tpe: Type, pos: SrcPos, kind: TypeKind)(using Context): Unit =

    def typeDescr = kind match
      case TypeKind.Result(sym, inferred) =>
        def inferredStr = if inferred then " inferred" else ""
        def resultStr = if sym.info.isInstanceOf[MethodicType] then " result" else ""
        i" $sym's$inferredStr$resultStr"
      case TypeKind.Argument =>
        " the argument's adapted type"

    def explicitRefs(tp: Type): Refs = tp match
      case tp: (TermRef | ThisType) => SimpleIdentitySet(tp)
      case AnnotatedType(parent, _) => explicitRefs(parent)
      case AndType(tp1, tp2) => explicitRefs(tp1) ++ explicitRefs(tp2)
      case OrType(tp1, tp2) => explicitRefs(tp1) ** explicitRefs(tp2)
      case _ => emptySet

    def prune(refs: Refs): Refs =
      refs.deductSym(kind.dclSym) -- explicitRefs(tpe)

    def checkParts(parts: List[Type]): Unit =
      var footprint: Refs = emptySet
      var hiddenSet: Refs = emptySet
      var checked = 0
      for part <- parts do

        /** Report an error if `current` and `next` overlap.
         *  @param  current  the footprint or hidden set seen so far
         *  @param  next     the footprint or hidden set of the next part
         *  @param  mapRefs  a function over the capture set elements of the next part
         *                   that returns the references of the same kind as `current`
         *                   (i.e. the part's footprint or hidden set)
         *  @param  prevRel  a verbal description of current ("references or "hides")
         *  @param  nextRel  a verbal descriiption of next
         */
        def checkSep(current: Refs, next: Refs, mapRefs: Refs => Refs, prevRel: String, nextRel: String): Unit =
          val globalOverlap = current.overlapWith(next)
          if !globalOverlap.isEmpty then
            val (prevStr, prevRefs, overlap) = parts.iterator.take(checked)
              .map: prev =>
                val prevRefs = prune(mapRefs(prev.deepCaptureSet.elems).footprint)
                (i",  $prev , ", prevRefs, prevRefs.overlapWith(next))
              .dropWhile(_._3.isEmpty)
              .nextOption
              .getOrElse(("", current, globalOverlap))
            report.error(
              em"""Separation failure in$typeDescr type $tpe.
                  |One part,  $part , $nextRel  ${CaptureSet(next)}.
                  |A previous part$prevStr $prevRel  ${CaptureSet(prevRefs)}.
                  |The two sets overlap at  ${CaptureSet(overlap)}.""",
              pos)

        val partRefs = part.deepCaptureSet.elems
        val partFootprint = prune(partRefs.footprint)
        val partHidden = prune(partRefs.hidden.footprint) -- partFootprint

        checkSep(footprint, partHidden, identity, "references", "hides")
        checkSep(hiddenSet, partHidden, _.hidden, "also hides", "hides")
        checkSep(hiddenSet, partFootprint, _.hidden, "hides", "references")

        footprint ++= partFootprint
        hiddenSet ++= partHidden
        checked += 1
      end for
    end checkParts

    object traverse extends TypeAccumulator[Captures]:

      /** A stack of part lists to check. We maintain this since immediately
       *  checking parts when traversing the type would check innermost to oputermost.
       *  But we want to check outermost parts first since this prioritized errors
       *  that are more obvious.
       */
      var toCheck: List[List[Type]] = Nil

      private val seen = util.HashSet[Symbol]()

      def apply(c: Captures, t: Type) =
        if variance < 0 then c
        else
          val t1 = t.dealias
          t1 match
            case t @ AppliedType(tycon, args) =>
              val c1 = foldOver(Captures.None, t)
              if c1 == Captures.NeedsCheck then
                toCheck = (tycon :: args) :: toCheck
              c.add(c1)
            case t @ CapturingType(parent, cs) =>
              val c1 = this(c, parent)
              if cs.elems.containsHidden then c1.add(Captures.Hidden)
              else if !cs.elems.isEmpty then c1.add(Captures.Explicit)
              else c1
            case t: TypeRef if t.symbol.isAbstractOrParamType =>
              if seen.contains(t.symbol) then c
              else
                seen += t.symbol
                apply(apply(c, t.prefix), t.info.bounds.hi)
            case t =>
              foldOver(c, t)

    def checkParameters() =
      val badParams = mutable.ListBuffer[Symbol]()
      def currentOwner = kind.dclSym.orElse(ctx.owner)
      for hiddenRef <- prune(tpe.deepCaptureSet.elems.hidden.footprint) do
        val refSym = hiddenRef.termSymbol
        if refSym.is(TermParam)
          && !refSym.hasAnnotation(defn.ConsumeAnnot)
          && !refSym.info.derivesFrom(defn.Caps_SharedCapability)
          && currentOwner.isContainedIn(refSym.owner)
        then
          badParams += refSym
      if badParams.nonEmpty then
        def paramsStr(params: List[Symbol]): String = (params: @unchecked) match
          case p :: Nil => i"${p.name}"
          case p :: p2 :: Nil => i"${p.name} and ${p2.name}"
          case p :: ps => i"${p.name}, ${paramsStr(ps)}"
        val (pluralS, singleS) = if badParams.tail.isEmpty then ("", "s") else ("s", "")
        report.error(
          em"""Separation failure:$typeDescr type $tpe hides parameter$pluralS ${paramsStr(badParams.toList)}
              |The parameter$pluralS need$singleS to be annotated with @consume to allow this.""",
            pos)

    def flagHiddenParams =
      kind match
        case TypeKind.Result(sym, _) =>
          !sym.isAnonymousFunction // we don't check return types of anonymous functions
          && !sym.is(Case)         // We don't check so far binders in patterns since they
                                   // have inferred universal types. TODO come back to this;
                                   // either infer more precise types for such binders or
                                   // "see through them" when we look at hidden sets.
        case TypeKind.Argument =>
          false

    if !tpe.hasAnnotation(defn.UntrackedCapturesAnnot) then
      traverse(Captures.None, tpe)
      traverse.toCheck.foreach(checkParts)
      if flagHiddenParams then checkParameters()
  end checkType

  private def collectMethodTypes(tp: Type): List[TermLambda] = tp match
    case tp: MethodType => tp :: collectMethodTypes(tp.resType)
    case tp: PolyType => collectMethodTypes(tp.resType)
    case _ => Nil

  private def dependencies(fn: Tree, argss: List[List[Tree]])(using Context): collection.Map[Tree, List[Tree]] =
    val mtpe =
      if fn.symbol.exists then fn.symbol.info
      else fn.tpe.widen // happens for PolyFunction applies
    val mtps = collectMethodTypes(mtpe)
    assert(mtps.hasSameLengthAs(argss), i"diff for $fn: ${fn.symbol} /// $mtps /// $argss")
    val mtpsWithArgs = mtps.zip(argss)
    val argMap = mtpsWithArgs.toMap
    val deps = mutable.HashMap[Tree, List[Tree]]().withDefaultValue(Nil)
    for
      (mt, args) <- mtpsWithArgs
      (formal, arg) <- mt.paramInfos.zip(args)
      dep <- formal.captureSet.elems.toList
    do
      val referred = dep.stripReach match
        case dep: TermParamRef =>
          argMap(dep.binder)(dep.paramNum) :: Nil
        case dep: ThisType if dep.cls == fn.symbol.owner =>
          val Select(qual, _) = fn: @unchecked
          qual :: Nil
        case _ =>
          Nil
      deps(arg) ++= referred
    deps

  private def traverseApply(tree: Tree, argss: List[List[Tree]])(using Context): Unit = tree match
    case Apply(fn, args) => traverseApply(fn, args :: argss)
    case TypeApply(fn, args) => traverseApply(fn, argss) // skip type arguments
    case _ =>
      if argss.nestedExists(_.needsSepCheck) then
        checkApply(tree, argss.flatten, dependencies(tree, argss))

  def traverse(tree: Tree)(using Context): Unit =
    tree match
      case tree: Apply if tree.symbol == defn.Caps_unsafeAssumeSeparate => return
      case _ =>
    checkUse(tree)
    tree match
      case tree: GenericApply =>
        tree.tpe match
          case _: MethodOrPoly =>
          case _ => traverseApply(tree, Nil)
        traverseChildren(tree)
      case tree: Block =>
        val saved = defsShadow
        previousDefs = mutable.ListBuffer() :: previousDefs
        try traverseChildren(tree)
        finally
          previousDefs = previousDefs.tail
          defsShadow = saved
      case tree: ValOrDefDef =>
        traverseChildren(tree)
        if !tree.symbol.isOneOf(TermParamOrAccessor) then
          checkType(tree.tpt, tree.symbol)
          if previousDefs.nonEmpty then
            capt.println(i"sep check def ${tree.symbol}: ${tree.tpt} with ${captures(tree.tpt).hidden.footprint}")
            defsShadow ++= captures(tree.tpt).hidden.footprint.deductSym(tree.symbol)
            resultType(tree.symbol) = tree.tpt.nuType
            previousDefs.head += tree
      case _ =>
        traverseChildren(tree)
end SepChecker






