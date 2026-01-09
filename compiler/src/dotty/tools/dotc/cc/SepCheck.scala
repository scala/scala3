package dotty.tools
package dotc
package cc
import ast.tpd
import collection.mutable

import core.*
import Symbols.*, Types.*, Flags.*, Contexts.*, Names.*, Decorators.*
import CaptureSet.{Refs, emptyRefs, HiddenSet}
import NameKinds.WildcardParamName
import config.Printers.capt
import StdNames.nme
import util.{SimpleIdentitySet, EqHashMap, SrcPos}
import tpd.*
import reflect.ClassTag
import reporting.trace
import Capabilities.*

/** The separation checker is  a tree traverser that is run after capture checking.
 *  It checks tree nodes for various separation conditions, explained in the
 *  methods below. Rough summary:
 *
 *   - Hidden sets of arguments must not be referred to in the same application
 *   - Hidden sets of (result-) types must not be referred to alter in the same scope.
 *   - Returned hidden sets can only refer to consume parameters.
 *   - If returned hidden sets refer to an encloding this, the reference must be
 *     from a consume method.
 *   - Consumed entities cannot be used subsequently.
 *   - Entitites cannot be consumed in a loop.
 */
object SepCheck:

  /** Enumerates kinds of captures encountered so far */
  enum Captures derives CanEqual:
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

  /** The role in which a checked type appears, used for composing error messages */
  enum TypeRole derives CanEqual:
    case Result(sym: Symbol, inferred: Boolean)
    case Argument(arg: Tree, meth: Symbol)
    case Qualifier(qual: Tree, meth: Symbol)
    case RHS(rhs: Tree, mvar: Symbol)

    /** If this is a Result tole, the associated symbol, otherwise NoSymbol */
    def dclSym = this match
      case Result(sym, _) => sym
      case _ => NoSymbol

    /** A textual description of this role */
    def description(using Context): String = this match
      case Result(sym, inferred) =>
        def inferredStr = if inferred then " inferred" else ""
        def resultStr = if sym.info.isInstanceOf[MethodicType] then " result" else ""
        i"${sym.sanitizedDescription}'s$inferredStr$resultStr type"
      case TypeRole.Argument(_, _) =>
        "the argument's adapted type"
      case TypeRole.Qualifier(_, meth) =>
        i"the type of the prefix to a call of ${meth.sanitizedDescription}"
      case RHS(rhs, lhsSym) =>
        i"the type of the value assigned to $lhsSym"

    /** A description how a reference was consumed in this role */
    def howConsumed(using Context): String = this match
      case TypeRole.Result(meth, _) => i"returned in the result of ${meth.sanitizedDescription}"
      case TypeRole.Argument(_, meth) => i"passed as a consume parameter to ${meth.sanitizedDescription}"
      case TypeRole.Qualifier(_, meth) => i"used as a prefix to consume ${meth.sanitizedDescription}"
      case TypeRole.RHS(_, mvar) => i"consumed in an assignment to $mvar"
  end TypeRole

  /** A class for segmented sets of consumed references.
   *  References are associated with the source positions where they first appeared.
   *  References are compared with `eq`.
   */
  abstract class ConsumedSet:
    /** The references in the set. The array should be treated as immutable in client code */
    def refs: Array[Capability]

    /** The associated source positoons and type roles. The array should be treated as immutable in client code */
    def locs: Array[(SrcPos, TypeRole)]

    /** The number of references in the set */
    def size: Int

    def toMap: Map[Capability, (SrcPos, TypeRole)] = refs.take(size).zip(locs).toMap

    def show(using Context) =
      s"[${toMap.map((ref, loc) => i"$ref -> $loc").toList}]"
  end ConsumedSet

  /** A fixed consumed set consisting of the given references `refs` and
   *  associated source positions `locs`
   */
  class ConstConsumedSet(val refs: Array[Capability], val locs: Array[(SrcPos, TypeRole)]) extends ConsumedSet:
    def size = refs.size

  /** A mutable consumed set, which is initially empty */
  class MutConsumedSet extends ConsumedSet:
    var refs: Array[Capability] = new Array(4)
    var locs: Array[(SrcPos, TypeRole)] = new Array(4)
    var size = 0
    var directPeaks : Refs = emptyRefs

    private def double[T <: AnyRef : ClassTag](xs: Array[T]): Array[T] =
      val xs1 = new Array[T](xs.length * 2)
      xs.copyToArray(xs1)
      xs1

    private def ensureCapacity(added: Int): Unit =
      if size + added > refs.length then
        refs = double(refs)
        locs = double(locs)

    /** If `ref` is in the set, its associated source position, otherwise `null` */
    def get(ref: Capability): (SrcPos, TypeRole) | Null =
      var i = 0
      while i < size && (refs(i) ne ref) do i += 1
      if i < size then locs(i) else null

    def clashing(ref: Capability)(using Context): (SrcPos, TypeRole) | Null =
      val refPeaks = ref.directPeaks
      if !directPeaks.sharedPeaks(refPeaks).isEmpty then
        var i = 0
        while i < size && refs(i).directPeaks.sharedPeaks(refPeaks).isEmpty do
          i += 1
        assert(i < size)
        locs(i)
      else null

    /** If `ref` is not yet in the set, add it with given source position */
    def put(ref: Capability, loc: (SrcPos, TypeRole))(using Context): Unit =
      if get(ref) == null then
        ensureCapacity(1)
        refs(size) = ref
        locs(size) = loc
        size += 1
        directPeaks  = directPeaks  ++ ref.directPeaks

    /** Add all references with their associated positions from `that` which
     *  are not yet in the set.
     */
    def ++= (that: ConsumedSet)(using Context): Unit =
      for i <- 0 until that.size do put(that.refs(i), that.locs(i))

    /** Run `op` and return any new references it created in a separate `ConsumedSet`.
     *  The current mutable set is reset to its state before `op` was run.
     */
    def segment(op: => Unit): ConsumedSet =
      val start = size
      val savedPeaks = directPeaks
      try
        op
        if size == start then EmptyConsumedSet
        else ConstConsumedSet(refs.slice(start, size), locs.slice(start, size))
      finally
        size = start
        directPeaks  = savedPeaks
  end MutConsumedSet

  val EmptyConsumedSet = ConstConsumedSet(Array(), Array())

  case class PeaksPair(actual: Refs, hidden: Refs)

  case class DefInfo(tree: ValOrDefDef, symbol: Symbol, hidden: Refs, hiddenPeaks: Refs)

  extension (refs: Refs)

    private def peaks(using Context): Refs =
      refs.filter(_.isTerminalCapability)

    private def nonPeaks(using Context): Refs =
      refs.filter(!_.isTerminalCapability)

    /** The footprint of a set of capabilities `refs` is the closure
     *  of `refs` under `_.captureSetOfInfo`, dropping any shared terminal
     *  capabilities. If `followHidden` is true, we close under both
     *  `_.captureSetOfInfo` and `_.hiddenElems`.
     */
    private def computeFootprint(followHidden: Boolean)(using Context): Refs =
      def recur(seen: Refs, acc: Refs, newElems: List[Capability]): Refs = trace(i"peaks $acc, $newElems = "):
        newElems match
        case newElem :: newElems1 =>
          if seen.contains(newElem) then
            recur(seen, acc, newElems1)
          else newElem.stripRestricted.stripReadOnly match
            case _: LocalCap if !newElem.isKnownClassifiedAs(defn.Caps_SharedCapability) =>
              val hiddens = if followHidden then newElem.hiddenSet.toList else Nil
              recur(seen + newElem, acc + newElem, hiddens ++ newElems1)
            case _ if newElem.isTerminalCapability =>
              recur(seen + newElem, acc, newElems1)
            case _ =>
              recur(seen + newElem, acc + newElem, newElem.captureSetOfInfo.dropEmpties().elems.toList ++ newElems1)
        case Nil => acc
      recur(emptyRefs, emptyRefs, refs.toList)

    /** The direct footprint of a set of capabilities `refs` is the closure
     *  of `refs` under `_.captureSetOfInfo`, dropping any shared terminal
     *  capabilities.
     */
    private def directFootprint(using Context): Refs =
      computeFootprint(followHidden = false)

    /** The complete footprint of a set of capabilities `refs` is the closure
     *  of `refs` under `_.captureSetOfInfo` and `_.hiddenElems`, dropping any shared terminal
     *  capabilities.
     */
    private def completeFootprint(using Context): Refs =
      computeFootprint(followHidden = true)

    /** Same as completeFootprint.peaks under new scheme. Was maximal elements before */
    private def allPeaks(using Context): Refs =
      completeFootprint.peaks

    /** The shared elements between the peak sets `refs` and `other`.
     *  These are the core capabilities and LocalCap capabilities that appear
     *  in a (possibly classified or readOnly) version in both sets and that
     *  that appear in a non-readOnly version in at least one of the sets.
     */
    private def sharedPeaks(other: Refs)(using Context): Refs =
      def common(refs1: Refs, refs2: Refs) =
        var acc: Refs = emptyRefs
        refs1.foreach: ref =>
          if !ref.isReadOnly then
            val coreRef = ref.stripRestricted
            if refs2.exists(_.stripRestricted.stripReadOnly.coversLocalCap(coreRef)) then
              acc += coreRef
        acc
      assert(refs.forall(_.isTerminalCapability))
      assert(other.forall(_.isTerminalCapability))
      common(refs, other) ++ common(other, refs)

    /** The overlap of two footprint sets F1 and F2. This contains all exclusive references `r`
     *  such that one of the following is true:
     *   1.
     *      - one of the sets contains `r`
     *      - the other contains a capability `s` or `s.rd` where `s` covers `r`
     *   2.
     *      - one of the sets contains `r.rd`
     *      - the other contains a capability `s` where `s` covers `r`
     *
     *  @see covers in Capability
     */
    private def overlapWith(other: Refs)(using Context): Refs =
      val refs1 = refs
      val refs2 = other

      /** Exclusive capabilities in refs1 that are covered by exclusive or
       *  stripped read-only capabilties in refs2
       *  + stripped read-only capabilities in refs1 that are covered by an
       *    exclusive capability in refs2.
       */
      def common(refs1: Refs, refs2: Refs) =
        refs1.filter: ref =>
          ref.isExclusive() && refs2.exists(_.stripReadOnly.covers(ref))
        ++
        refs1
          .filter:
            case ReadOnly(ref @ TermRef(prefix: CoreCapability, _)) =>
              // We can get away testing only references with at least one field selection
              // here since stripped readOnly references that equal a reference in refs2
              // are added by the first clause of the symmetric call to common.
              refs2.exists(_.covers(prefix))
            case _ =>
              false
          .map(_.stripReadOnly)

      common(refs, other) ++ common(other, refs)
    end overlapWith

   /** Reduce a non-empty footprint set to
     *   1. all its non-terminial capabilities if that set is nonempty, or
     *   2. all its non-hidden capabilities if that set is nonempty, or
     *   3. the set itself if it consists only of hidden terminal capabilities.
     */
    def reduced(using Context): Refs =
      val concrete = refs.nonPeaks
      if !concrete.isEmpty then concrete
      else
        val notHidden = refs -- refs.flatMap(_.hiddenSet)
        if !notHidden.isEmpty then notHidden
        else refs

    /** The non-terminal elements hidden directly or indirectly by a terminal
     *  capability in `refs`. E g. if `R = {x, <any hiding <y, <any hiding z>>}` then
     *  its hidden set is `{y, z}`.
     */
    private def transHiddenSet(using Context): Refs =
      val seen: util.EqHashSet[Capability] = new util.EqHashSet

      def recur(refs: Refs): Refs =
        (emptyRefs /: refs): (elems, elem) =>
          if seen.add(elem)
          then elems ++ elem.computeHiddenSet(refs => refs ++ recur(refs))
          else elems

      recur(refs)
    end transHiddenSet

    /** Subtract all elements that are covered by some element in `others` from this set. */
    private def deduct(others: Refs)(using Context): Refs =
      refs.filter: ref =>
        !others.exists(_.covers(ref))

    /** Deduct `sym` and `sym*` from `refs` */
    private def deductSymRefs(sym: Symbol)(using Context): Refs =
      val ref = sym.termRef
      if ref.isTrackableRef then refs.deduct(SimpleIdentitySet(ref, ref.reach))
      else refs

  end extension

  extension (ref: Capability)
    def directPeaks (using Context): Refs =
      SimpleIdentitySet(ref).directFootprint.peaks

class SepCheck(checker: CheckCaptures.CheckerAPI) extends tpd.TreeTraverser:
  import checker.*
  import SepCheck.*

  /** The set of capabilities that are hidden by a polymorphic result type
   *  of some previous definition.
   */
  private var defsShadow: Refs = emptyRefs

  /** The previous val or def definitions encountered during separation checking
   *  in reverse order. These all enclose and precede the current traversal node.
   */
  private var previousDefs: List[DefInfo] = Nil

  /** The set of references that were consumed so far in the current method */
  private var consumed: MutConsumedSet = MutConsumedSet()

  /** Infos aboput Labeled expressions enclosing the current traversal point.
   *  For each labeled expression, it's label name, and a list buffer containing
   *  all consumed sets of return expressions referring to that label.
   */
  private var openLabeled: List[(Name, mutable.ListBuffer[ConsumedSet])] = Nil

  /** The deep capture set of an argument or prefix widened to the formal parameter, if
   *  the latter contains an `any`.
   */
  private def formalCaptures(arg: Tree)(using Context): Refs =
    arg.formalType.orElse(arg.nuType).spanCaptureSet.elems

   /** The span capture set of the type of `tree` */
  private def spanCaptures(tree: Tree)(using Context): Refs =
   tree.nuType.spanCaptureSet.elems

   /** The deep capture set of the type of `tree` */
  private def deepCaptures(tree: Tree)(using Context): Refs =
   tree.nuType.deepCaptureSet.elems

  // ---- Error reporting TODO Once these are stabilized, move to messages -----" +

  def overlapStr(hiddenSet: Refs, clashSet: Refs)(using Context): String =
    val hiddenFootprint = hiddenSet.directFootprint
    val clashFootprint = clashSet.directFootprint
    val shared = hiddenFootprint.overlapWith(clashFootprint).reduced
    if shared.isEmpty then i"${CaptureSet(shared)}"
    else shared.nth(0) match
      case localCap: LocalCap =>
        if ctx.settings.YccVerbose.value then i"{$localCap}"
        else i"{`$localCap` of ${localCap.ccOwnerStr}}"
      case _ =>
        i"${CaptureSet(shared)}"

  /** Report a separation failure in an application `fn(args)`
   *  @param fn          the function
   *  @param parts       the function prefix followed by the flattened argument list
   *  @param polyArg     the clashing argument to a polymorphic formal
   *  @param clashing    the argument, function prefix, or entire function application result with
   *                     which it clashes,
   *
   */
  def sepApplyError(fn: Tree, parts: List[Tree], polyArg: Tree, clashing: Tree)(using Context): Unit =
    val polyArgIdx = parts.indexOf(polyArg).ensuring(_ >= 0) - 1
    val clashIdx = parts.indexOf(clashing) // -1 means entire function application
    def paramName(mt: Type, idx: Int): Option[Name] = mt match
      case mt @ MethodType(pnames) =>
        if idx < pnames.length then Some(pnames(idx)) else paramName(mt.resType, idx - pnames.length)
      case mt: PolyType => paramName(mt.resType, idx)
      case _ => None
    def formalName = paramName(fn.nuType.widen, polyArgIdx) match
      case Some(pname) => i"$pname "
      case _ => ""
    def qualifier = methPart(fn) match
      case Select(qual, _) => qual
      case _ => EmptyTree
    def isShowableMethod = fn.symbol.exists && !defn.isFunctionSymbol(fn.symbol.maybeOwner)
    def funType =
      if fn.symbol.exists && !qualifier.isEmpty then qualifier.nuType else fn.nuType
    def funStr =
      if isShowableMethod then i"${fn.symbol}: ${fn.symbol.info}"
      else i"a function of type ${funType.widen}"
    def clashArgStr = clashIdx match
      case -1 => "function result"
      case 0  => "function prefix"
      case 1  => "first argument "
      case 2  => "second argument"
      case 3  => "third argument "
      case n  => s"${n}th argument  "
    def clashTypeStr =
      if clashIdx == 0 && !isShowableMethod then "" // we already mentioned the type in `funStr`
      else i" with type  ${clashing.nuType}"
    val hiddenSet = formalCaptures(polyArg).transHiddenSet
    val clashSet = if clashIdx == -1 then deepCaptures(clashing) else spanCaptures(clashing)
    val hiddenFootprint = hiddenSet.completeFootprint
    val clashFootprint = clashSet.completeFootprint

    report.error(
      em"""Separation failure: argument of type  ${polyArg.nuType}
          |to $funStr
          |corresponds to capture-polymorphic formal parameter ${formalName}of type  ${polyArg.formalType}
          |and hides capabilities  ${CaptureSet(hiddenSet)}.
          |Some of these overlap with the captures of the ${clashArgStr.trim}$clashTypeStr.
          |
          |  Hidden set of current argument        : ${CaptureSet(hiddenSet)}
          |  Hidden footprint of current argument  : ${CaptureSet(hiddenFootprint.nonPeaks)}
          |  Capture set of $clashArgStr        : ${CaptureSet(clashSet)}
          |  Footprint set of $clashArgStr      : ${CaptureSet(clashFootprint.nonPeaks)}
          |  The two sets overlap at               : ${overlapStr(hiddenFootprint, clashFootprint)}""",
      polyArg.srcPos)

  /** Report a use/definition failure, where a previously hidden capability is
   *  used again.
   *  @param tree        the tree where the capability is used
   *  @param clashing    the tree where the capability is previously hidden,
   *                     or emptyTree if none exists
   *  @param used        the uses of `tree`
   *  @param hidden      the hidden set of the clashing def,
   *                     or the global hidden set if no clashing def exists
   */
  def sepUseError(tree: Tree, clashingDef: ValOrDefDef | Null, used: Refs, hidden: Refs)(using Context): Unit =
    if clashingDef != null then
      def resultStr = if clashingDef.isInstanceOf[DefDef] then " result" else ""
      //println(i"sep use error: previous ${clashingDef.tpt.nuType}, ref = $used")
      report.error(
        em"""Separation failure: Illegal access to ${overlapStr(hidden, used)} which is hidden by the previous definition
            |of ${clashingDef.symbol} with$resultStr type ${clashingDef.tpt.nuType}.
            |This type hides capabilities  ${CaptureSet(hidden)}""",
        tree.srcPos)
    else
      report.error(
        em"""Separation failure: illegal access to ${overlapStr(hidden, used)} which is hidden by some previous definitions
            |No clashing definitions were found. This might point to an internal error.""",
        tree.srcPos)

  /** Report a failure where a previously consumed capability is used again,
   *  @param ref     the capability that is used after being consumed
   *  @param loc     the position where the capability was consumed
   *  @param pos     the position where the capability was used again
   */
  def consumeError(ref: Capability, loc: (SrcPos, TypeRole), pos: SrcPos)(using Context): Unit =
    val (locPos, role) = loc
    report.error(
      em"""Separation failure: Illegal access to $ref, which was ${role.howConsumed}
          |on line ${locPos.line + 1} and therefore is no longer available.""",
      pos)

  /** Report a failure where a capability is consumed in a loop.
   *  @param ref     the capability
   *  @param pos     the position where the capability was consumed
   */
  def consumeInLoopError(ref: Capability, loc: (SrcPos, TypeRole))(using Context): Unit =
    val (pos, role) = loc
    report.error(
      em"""Separation failure: $ref appears in a loop, therefore it cannot
          |be ${role.howConsumed}.""",
      pos)

  // ------------ Checks -----------------------------------------------------

  /** Check separation between different arguments and between function
   *  prefix and arguments. A capability cannot be hidden by one of these arguments
   *  and also be either explicitly referenced or hidden by the prefix or another
   *  argument. "Hidden" means: the capability is in the deep capture set of the
   *  argument and appears in the hidden set of the corresponding (capture-polymorphic)
   *  formal parameter. Howeber, we do allow explicit references to a hidden
   *  capability in later arguments, if the corresponding formal parameter mentions
   *  the parameter where the capability was hidden. For instance in
   *
   *    def seq(x: () => Unit; y ->{any, x} Unit): Unit
   *    def f: () ->{io} Unit
   *
   *  we do allow `seq(f, f)` even though `{f, io}` is in the hidden set of the
   *  first parameter `x`, since the second parameter explicitly mentions `x` in
   *  its capture set.
   *
   *  Also check separation via checkType within individual arguments widened to their
   *  formal paramater types.
   *
   *  @param fn            the applied function
   *  @param args          the flattened argument lists
   *  @param app           the entire application tree
   *  @param deps          cross argument dependencies: maps argument trees to
   *                       those other arguments that where mentioned by coorresponding
   *                       formal parameters.
   *  @param resultPeaks   peaks in the result type that could interfere with the
   *                       hidden sets of formal parameters
   */
  private def checkApply(fn: Tree, args: List[Tree], app: Tree, deps: collection.Map[Tree, List[Tree]], resultPeaks: Refs)(using Context): Unit =
    val (qual, fnCaptures) = methPart(fn) match
      case Select(qual, _) => (qual, qual.nuType.captureSet)
      case _ => (fn, CaptureSet.empty)
    var currentPeaks = PeaksPair(fnCaptures.elems.allPeaks, emptyRefs)
    val partsWithPeaks = mutable.ListBuffer[(Tree, PeaksPair)]() += (qual -> currentPeaks)

    capt.println(
      i"""check separate $fn($args), fnCaptures = $fnCaptures,
         |  formalCaptures = ${args.map(arg => CaptureSet(formalCaptures(arg)))},
         |  actualCaptures = ${args.map(arg => CaptureSet(spanCaptures(arg)))},
         |  resultPeaks = ${resultPeaks},
         |  deps = ${deps.toList}""")
    val parts = qual :: args
    var reported: SimpleIdentitySet[Tree] = SimpleIdentitySet.empty

    for arg <- args do
      val argPeaks = PeaksPair(
          spanCaptures(arg).allPeaks,
          if arg.needsSepCheck then formalCaptures(arg).transHiddenSet.allPeaks else emptyRefs)
      val argDeps = deps(arg)

      def clashingPart(argPeaks: Refs, selector: PeaksPair => Refs): Tree =
        partsWithPeaks.find: (prev, prevPeaks) =>
          !argDeps.contains(prev)
          && !selector(prevPeaks).sharedPeaks(argPeaks).isEmpty
        match
          case Some(prev, _) => prev
          case None => EmptyTree

      // 1. test argPeaks.actual against previously captured hidden sets
      if !argPeaks.actual.sharedPeaks(currentPeaks.hidden).isEmpty then
        val clashing = clashingPart(argPeaks.actual, _.hidden)
        if !clashing.isEmpty then
          sepApplyError(fn, parts, clashing, arg)
          reported += clashing
        else assert(!argDeps.isEmpty)

      if arg.needsSepCheck then
        //println(i"testing $arg, formal = ${arg.formalType}, peaks = ${argPeaks.actual}/${argPeaks.hidden} against ${currentPeaks.actual}")
        checkType(arg.formalType, arg.srcPos, TypeRole.Argument(arg, fn.symbol))
        // 2. test argPeaks.hidden against previously captured actuals
        if !argPeaks.hidden.sharedPeaks(currentPeaks.actual).isEmpty then
          val clashing = clashingPart(argPeaks.hidden, _.actual)
          if !clashing.isEmpty then
            if !reported.contains(clashing) then
              //println(i"CLASH $arg / ${argPeaks.formal} vs $clashing / ${peaksOfTree(clashing).actual} / ${spanCaptures(clashing).peaks}")
              sepApplyError(fn, parts, arg, clashing)
          else assert(!argDeps.isEmpty)

      partsWithPeaks += (arg -> argPeaks)
      currentPeaks = PeaksPair(
          currentPeaks.actual ++ argPeaks.actual,
          currentPeaks.hidden ++ argPeaks.hidden)
    end for

    if !resultPeaks.isEmpty then
      lazy val partPeaks = partsWithPeaks.toMap
      for arg <- args do
        if arg.needsSepCheck && !partPeaks(arg).hidden.sharedPeaks(resultPeaks).isEmpty then
          sepApplyError(fn, parts, arg, app)
  end checkApply

  def checkAssign(tree: Assign)(using Context): Unit =
    val Assign(lhs, rhs) = tree
    lhs.nuType match
      case lhsType: ObjectCapability if lhsType.pathOwner.exists =>
        val lhsOwner = lhsType.pathOwner

        /** A reference escapes into an outer var if it would have produced a
         *  level error if it did not have an Unscoped, unprefixed LocalCap
         *  in some underlying capture set.
         */
        def escapes(ref: Capability): Boolean = ref.pathRoot match
          case ref @ LocalCap(NoPrefix)
          if ref.classifier.derivesFrom(defn.Caps_Unscoped) =>
            // we have an escaping reference if the LocalCap's adjustded owner
            // is properly contained inside the scope of the variable.
            ref.adjustOwner(ref.ccOwner).isProperlyContainedIn(lhsOwner)
          case _ =>
            ref.visibility.isProperlyContainedIn(lhsOwner) // ref itself is not levelOK
            && !ref.isTerminalCapability                   // ... and ...
            && ref.captureSetOfInfo.elems.exists(escapes)  // some underlying capability escapes

        val escaping = spanCaptures(rhs).filter(escapes)
        capt.println(i"check assign $tree, $lhsOwner, escaping = $escaping, ${escaping.directFootprint.nonPeaks}")
        checkConsumedRefs(escaping.directFootprint.nonPeaks, rhs.nuType,
          TypeRole.RHS(rhs, lhs.symbol),
          s"the value assigned to ${lhs.symbol} refers to", tree.srcPos)
      case _ =>
  end checkAssign

  /** 1. Check that the capabilities used at `tree` don't overlap with
   *     capabilities hidden by a previous definition.
   *  2. Also check that none of the used capabilities was consumed before.
   */
  def checkUse(tree: Tree)(using Context): Unit =
    val used = tree.markedFree.elems
    if !used.isEmpty then
      capt.println(i"check use $tree: $used")
      val usedPeaks = used.allPeaks
      if !defsShadow.allPeaks.sharedPeaks(usedPeaks).isEmpty then
        // Drop all Selects unless they select from a `this`
        def pathRoot(tree: Tree): Tree = tree match
          case Select(This(_), _) => tree
          case Select(prefix, _) => pathRoot(prefix)
          case _ => tree

        val rootSym = pathRoot(tree).symbol

        def findClashing(prevDefs: List[DefInfo]): Option[DefInfo] = prevDefs match
          case prevDef :: prevDefs1 =>
            if prevDef.symbol == rootSym then Some(prevDef)
            else if !prevDef.hiddenPeaks.sharedPeaks(usedPeaks).isEmpty then Some(prevDef)
            else findClashing(prevDefs1)
          case Nil =>
            None

        findClashing(previousDefs) match
          case Some(clashing) =>
            //println(i"check use $tree, $used, $rootSym, ${clashing.symbol}")
            if clashing.symbol != rootSym then
              sepUseError(tree, clashing.tree, used, clashing.hidden)
          case None =>
            sepUseError(tree, null, used, defsShadow)
      end if

      for ref <- used do
        val loc = consumed.clashing(ref)
        if loc != null then
          // println(i"consumed so far ${consumed.refs.toList} with peaks ${consumed.directPeaks.toList}, used = $used, exposed = ${ref.directPeaks }")
          consumeError(ref, loc, tree.srcPos)
  end checkUse

  /** If `tp` denotes some version of a singleton capability `x.type` the set `{x, x*}`
   *  otherwise the empty set.
   */
  def explicitRefs(tp: Type)(using Context): Refs = tp match
    case tp: (TermRef | ThisType) if tp.isTrackableRef => SimpleIdentitySet(tp, tp.reach)
    case AnnotatedType(parent, _) => explicitRefs(parent)
    case AndType(tp1, tp2) => explicitRefs(tp1) ++ explicitRefs(tp2)
    case OrType(tp1, tp2) => explicitRefs(tp1) ** explicitRefs(tp2)
    case _ => emptyRefs

  /** Check validity of consumed references `refsToCheck`. The references are consumed
   *  because they are hidden in a LocalCap result type or they are referred
   *  to in an argument to a consume parameter or in a prefix of a consume method --
   *  which one applies is determined by the role parameter.
   *
   *  This entails the following checks:
   *   - The reference must be defined in the same method or class as
   *     the access.
   *   - If the reference is to a term parameter, that parameter must be
   *     marked as consume as well.
   *   - If the reference is to a this type of the enclosing class, the
   *     access must be in a consume method.
   *
   *  References that extend caps.Sharable are excluded from checking.
   *  As a side effect, add all checked references with the given position `pos`
   *  to the global `consumed` map.
   *
   *  @param refsToCheck   the references to check
   *  @param tpe           the type containing those references
   *  @param role          the role in which the type apears
   *  @param descr         a textual description of the type and its relationship with the checked reference
   *  @param pos           position for error reporting
   */
  def checkConsumedRefs(refsToCheck: Refs, tpe: Type, role: TypeRole, descr: => String, pos: SrcPos)(using Context) =
    val badParams = mutable.ListBuffer[Symbol]()
    def currentOwner = role.dclSym.orElse(ctx.owner)
    for hiddenRef <- refsToCheck.deduct(explicitRefs(tpe)) do
      if !hiddenRef.stripReadOnly.isKnownClassifiedAs(defn.Caps_SharedCapability) then
        hiddenRef.pathRoot match
          case ref: TermRef if ref.symbol != role.dclSym =>
            val refSym = ref.symbol
            if currentOwner.enclosingMethodOrClass.isProperlyContainedIn(refSym.maybeOwner.enclosingMethodOrClass) then
              report.error(em"""Separation failure: $descr non-local $refSym""", pos)
            else if refSym.is(TermParam)
              && !refSym.isConsumeParam
              && currentOwner.isContainedIn(refSym.owner)
            then
              badParams += refSym
          case ref: ThisType =>
            val encl = currentOwner.enclosingMethodOrClass
            if encl.isProperlyContainedIn(ref.cls)
                && !encl.is(Synthetic)
                && !encl.hasAnnotation(defn.ConsumeAnnot)
            then
              report.error(
                em"""Separation failure: $descr non-local this of class ${ref.cls}.
                    |The access must be in a consume method to allow this.""",
                pos)
          case _ =>

    if badParams.nonEmpty then
      def paramsStr(params: List[Symbol]): String = (params: @unchecked) match
        case p :: Nil => i"${p.name}"
        case p :: p2 :: Nil => i"${p.name} and ${p2.name}"
        case p :: ps => i"${p.name}, ${paramsStr(ps)}"
      val (pluralS, singleS) = if badParams.tail.isEmpty then ("", "s") else ("s", "")
      report.error(
        em"""Separation failure: $descr parameter$pluralS ${paramsStr(badParams.toList)}.
            |The parameter$pluralS need$singleS to be annotated with consume to allow this.""",
          pos)

    role match
      case _: TypeRole.Argument | _: TypeRole.Qualifier | _: TypeRole.RHS =>
        for ref <- refsToCheck do
          if !ref.stripReadOnly.isKnownClassifiedAs(defn.Caps_SharedCapability) then
            consumed.put(ref, (pos, role))
      case _ =>
  end checkConsumedRefs

  /** Check separation conditions of type `tpe` that appears in `role`.
   *   1. Check that the parts of type `tpe` are mutually separated, as defined in
   *      `checkParts` below.
   *   2. Check that validity of all references consumed by the type as defined in
   *      `checkLegalRefs` below
   */
  def checkType(tpe: Type, pos: SrcPos, role: TypeRole)(using Context): Unit =

    /** Deduct some elements from `refs` according to the role of the checked type `tpe`:
     *   - If the the type apears as a (result-) type of a definition of `x`, deduct
     *       `x` and `x*`.
     *   - If the checked type (or, for arguments, the actual type of the argument)
     *     is morally a singleton type `y.type` deduct `y` and `y*` as well.
     */
    extension (refs: Refs) def pruned =
      val deductedType = role match
        case TypeRole.Argument(arg, _) => arg.tpe
        case _ => tpe
      refs.deductSymRefs(role.dclSym).deduct(explicitRefs(deductedType))

    def sepTypeError(parts: List[Type], genPart: Type, otherPart: Type): Unit =
      val captured = genPart.deepCaptureSet.elems
      val hiddenSet = captured.transHiddenSet.pruned
      val clashSet = otherPart.deepCaptureSet.elems
      var deepClashSet = clashSet.completeFootprint.nonPeaks.pruned
      if deepClashSet.isEmpty then
        deepClashSet = clashSet.completeFootprint.pruned
      report.error(
        em"""Separation failure in ${role.description} $tpe.
            |One part,  $genPart, hides capabilities  ${CaptureSet(hiddenSet)}.
            |Another part,  $otherPart,  captures capabilities  ${CaptureSet(deepClashSet)}.
            |The two sets overlap at  ${overlapStr(hiddenSet, deepClashSet)}.""",
        pos)

    /** Check that the parts of type `tpe` are mutually separated.
     *  This means that references hidden in some part of the type may not
     *  be explicitly referenced or hidden in some other part.
     */
    def checkParts(parts: List[Type]): Unit =
      var currentPeaks = PeaksPair(emptyRefs, emptyRefs)
      val partsWithPeaks = mutable.ListBuffer[(Type, PeaksPair)]()

      for part <- parts do
        val captured = part.deepCaptureSet.elems.pruned
        val hidden = captured.transHiddenSet.pruned
        val actual = captured ++ hidden
        val partPeaks = PeaksPair(actual.allPeaks, hidden.allPeaks)
        /*
        println(i"""check parts $parts
                   |current = ${currentPeaks.actual}/${currentPeaks.hidden}
                   |new = $captured/${captured.hiddenSet.pruned}
                   |new = ${captured.peaks}/${captured.hiddenSet.pruned.peaks}""")
        */

        def clashingPart(argPeaks: Refs, selector: PeaksPair => Refs): Type =
          partsWithPeaks.find: (prev, prevPeaks) =>
            !selector(prevPeaks).sharedPeaks(argPeaks).isEmpty
          match
            case Some(prev, _) => prev
            case None => NoType

        if !partPeaks.actual.sharedPeaks(currentPeaks.hidden).isEmpty then
          //println(i"CLASH ${partPeaks.actual} with ${currentPeaks.hidden}")
          val clashing = clashingPart(partPeaks.actual, _.hidden)
          //println(i"CLASH ${partPeaks.actual} with ${currentPeaks.hidden}")
          if clashing.exists then sepTypeError(parts, clashing, part)

        if !partPeaks.hidden.sharedPeaks(currentPeaks.actual).isEmpty then
          val clashing = clashingPart(partPeaks.hidden, _.actual)
          if clashing.exists then sepTypeError(parts, part, clashing)

        partsWithPeaks += (part -> partPeaks)
        currentPeaks = PeaksPair(
            currentPeaks.actual ++ partPeaks.actual,
            currentPeaks.hidden ++ partPeaks.hidden)
    end checkParts

    /** A traverser that collects part lists to check for separation conditions.
     *  The accumulator of type `Captures` indicates what kind of captures were
     *  encountered in previous parts.
     */
    object traverse extends TypeAccumulator[Captures]:

      /** A stack of part lists to check. We maintain this since immediately
       *  checking parts when traversing the type would check innermost to outermost.
       *  But we want to check outermost parts first since this prioritizes errors
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
              if cs.elems.exists(_.core.isInstanceOf[LocalCap]) then c1.add(Captures.Hidden)
              else if !cs.elems.isEmpty then c1.add(Captures.Explicit)
              else c1
            case t: TypeRef if t.symbol.isAbstractOrParamType =>
              if seen.contains(t.symbol) then c
              else
                seen += t.symbol
                apply(apply(c, t.prefix), t.info.bounds.hi)
            case t =>
              foldOver(c, t)

    /** Check that no LocalCaps from the bounds of parameters or abstract types
     *  are hidden in the result. See issue #24539
     */
    def checkNoTParamBounds(refsToCheck: Refs, descr: => String, pos: SrcPos): Unit =
      for ref <- refsToCheck do
        ref match
          case ref: LocalCap =>
            ref.origin match
              case Origin.InDecl(sym) if sym.isAbstractOrParamType =>
                report.error(
                  em"""Separation failure: $descr $ref, which appears in the bound of $sym.
                      |This is not allowed. The $sym has to be returned explicitly in the result type.""",
                  pos)
              case _ =>
          case _ =>

    /** If `tpe` appears as a (result-) type of a definition, treat its
     *  hidden set minus its explicitly declared footprint as consumed.
     *  If `tpe` appears as an argument to a consume parameter, treat
     *  its footprint as consumed.
     */
    def checkLegalRefs() = role match
      case TypeRole.Result(sym, _) =>
        if !sym.isAnonymousFunction // we don't check return types of anonymous functions
            && !sym.is(Case)        // We don't check so far binders in patterns since they
                                    // have inferred universal types. TODO come back to this;
                                    // either infer more precise types for such binders or
                                    // "see through them" when we look at hidden sets.
        then
          val refs = tpe.deepCaptureSet.elems
          val refsStar = refs.transHiddenSet
          val toCheck = refsStar.directFootprint.nonPeaks.deduct(refs.directFootprint.nonPeaks)
          def descr = i"${role.description} $tpe hides"
          checkConsumedRefs(toCheck, tpe, role, descr, pos)
          checkNoTParamBounds(refsStar, descr, pos)
      case TypeRole.Argument(arg, _) =>
        if tpe.hasAnnotation(defn.ConsumeAnnot) then
          val capts = spanCaptures(arg).directFootprint.nonPeaks
          checkConsumedRefs(capts, tpe, role, i"argument to consume parameter with type ${arg.nuType} refers to", pos)
      case _ =>

    if !tpe.hasAnnotation(defn.UntrackedCapturesAnnot) then
      traverse(Captures.None, tpe)
      traverse.toCheck.foreach(checkParts)
      checkLegalRefs()
  end checkType

  /** Check the (result-) type of a definition of symbol `sym` */
  def checkType(tpt: Tree, sym: Symbol)(using Context): Unit =
    checkType(tpt.nuType, tpt.srcPos,
        TypeRole.Result(sym, inferred = tpt.isInstanceOf[InferredTypeTree]))

  /** The list of all individual method types making up some potentially
   *  curried method type.
   */
  private def collectMethodTypes(tp: Type): List[TermLambda] = tp match
    case tp: MethodType => tp :: collectMethodTypes(tp.resType)
    case tp: PolyType => collectMethodTypes(tp.resType)
    case _ => Nil

  /** The inter-parameter dependencies of the function reference `fn` applied
   *  to the argument lists `argss`. For instance, if `f` has type
   *
   *    f(x: A, y: B^{any, x}, z: C^{x, y}): D
   *
   *  then the dependencies of an application `f(a, b, c)` of type C^{y} is the map
   *
   *    [ b -> [a]
   *    , c -> [a, b] ]
   *
   * It also returns the interfering peaks of the result of the application. They are the
   * peaks of argument captures and deep captures of the result function type, minus the
   * those dependent on parameters. For instance,
   * if `f` has the type (x: A, y: B, c: C) -> (op: () ->{b} Unit) -> List[() ->{x, y, a} Unit], its interfering
   * peaks will be the peaks of `a` and `b`.
   */
  private def dependencies(fn: Tree, argss: List[List[Tree]], app: Tree)(using Context): (collection.Map[Tree, List[Tree]], Refs) =
    def isFunApply(sym: Symbol) =
      sym.name == nme.apply && defn.isFunctionClass(sym.owner)
    val mtpe =
      if fn.symbol.exists && !isFunApply(fn.symbol) then fn.symbol.info
      else fn.nuType.widen
    val mtps = collectMethodTypes(mtpe)
    assert(mtps.hasSameLengthAs(argss), i"diff for $fn: ${fn.symbol} /// $mtps /// $argss")
    val mtpsWithArgs = mtps.zip(argss)
    val argMap = mtpsWithArgs.toMap
    val deps = mutable.LinkedHashMap[Tree, List[Tree]]().withDefaultValue(Nil)

    def argOfDep(dep: Capability): Option[Tree] =
      dep.stripReach match
        case dep: TermParamRef =>
          Some(argMap(dep.binder)(dep.paramNum))
        case dep: ThisType if dep.cls == fn.symbol.owner =>
          val Select(qual, _) = fn: @unchecked // TODO can we use fn instead?
          Some(qual)
        case _ =>
          None

    def recordDeps(formal: Type, actual: Tree) =
      def captures = formal.captureSet
      for dep <- captures.elems.toList do
        val referred = argOfDep(dep)
        deps(actual) ++= referred

    inline def isLocalRef(x: Capability): Boolean = x.isInstanceOf[TermParamRef]

    def resultArgCaptures(tpe: Type): Refs =
      def collectRefs(args: List[Type], res: Type) =
        args.foldLeft(resultArgCaptures(res)): (refs, arg) =>
          refs ++ arg.captureSet.elems
      tpe match
        case defn.FunctionOf(args, resultType, isContextual) =>
          collectRefs(args, resultType)
        case defn.RefinedFunctionOf(mt) =>
          collectRefs(mt.paramInfos, mt.resType)
        case CapturingType(parent, refs) =>
          resultArgCaptures(parent) ++ tpe.boxedCaptureSet.elems
        case _ =>
          emptyRefs

    for (mt, args) <- mtpsWithArgs; (formal, arg) <- mt.paramInfos.zip(args) do
      recordDeps(formal, arg)

    val resultType = mtpe.finalResultType
    val resultCaptures =
      (resultArgCaptures(resultType) ++ resultType.deepCaptureSet.elems).filter(!isLocalRef(_))
      // See i23726.scala why deepCaptureSet is needed here.
    val resultPeaks = resultCaptures.allPeaks
    capt.println(i"deps for $app = ${deps.toList}")
    (deps, resultPeaks)
  end dependencies

  /** Decompose an application into a function prefix and a list of argument lists.
   *  If some of the arguments need a separation check because they are capture polymorphic,
   *  perform a separation check with `checkApply`
   */
  private def traverseApply(app: Tree)(using Context): Unit =
    def recur(tree: Tree, argss: List[List[Tree]]): Unit = tree match
      case Apply(fn, args) => recur(fn, args :: argss)
      case TypeApply(fn, args) => recur(fn, argss) // skip type arguments
      case _ =>
        if argss.nestedExists(_.needsSepCheck) then
          val (deps, resultPeaks) = dependencies(tree, argss, app)
          checkApply(tree, argss.flatten, app, deps, resultPeaks)
    recur(app, Nil)

  /** Is `tree` an application of `caps.unsafe.unsafeAssumeSeparate`? */
  def isUnsafeAssumeSeparate(tree: Tree)(using Context): Boolean = tree match
    case tree: Apply => tree.symbol == defn.Caps_unsafeAssumeSeparate
    case _ => false

  def pushDef(tree: ValOrDefDef, hiddenByDef: Refs)(using Context): Unit =
    defsShadow ++= hiddenByDef
    previousDefs = DefInfo(tree, tree.symbol, hiddenByDef, hiddenByDef.allPeaks) :: previousDefs

  /** Check (result-) type of `tree` for separation conditions using `checkType`.
   *  Excluded are parameters and definitions that have an =unsafeAssumeSeparate
   *  application as right hand sides.
   *  Hidden sets of checked definitions are added to `defsShadow`.
   */
  def checkValOrDefDef(tree: ValOrDefDef)(using Context): Unit =
    if !tree.symbol.isOneOf(TermParamOrAccessor) && !isUnsafeAssumeSeparate(tree.rhs) then
      checkType(tree.tpt, tree.symbol)
      capt.println(i"sep check def ${tree.symbol}: ${tree.tpt} with ${spanCaptures(tree.tpt).transHiddenSet.directFootprint}")
      pushDef(tree, spanCaptures(tree.tpt).transHiddenSet.deductSymRefs(tree.symbol))

  def inSection[T](op: => T)(using Context): T =
    val savedDefsShadow = defsShadow
    val savedPrevionsDefs = previousDefs
    try op
    finally
      previousDefs = savedPrevionsDefs
      defsShadow = savedDefsShadow

  def traverseSection[T](tree: Tree)(using Context) = inSection(traverseChildren(tree))

  /** Should separatiion checking be disabled for the body of this method?
   */
  def skippable(sym: Symbol)(using Context): Boolean =
    sym.isInlineMethod
      // We currently skip inline method bodies since these seem to generate
      // spurious recheck completions. Test case is i20237.scala
    || sym.is(Synthetic) && sym.name.startsWith("_")
      // Approximation of case class getters _1, _2, ... . We can't separation check them
      // or colltest5/CollectionStrawManCC5_1.scala would fail with an error in
      // case class Filter. TODO Investigate to decide what needs to be done
      //  - Can we make the accessors work somehow?
      //  - If not, should we disable just accessors or all synthetic methods?
      // Reporting an error in a synthetic method is very frustrating since we don't have
      // a position with source code to show. On the other hand, skipping all synthetic members
      // might cause soundness issues.

  /** Traverse `tree` and perform separation checks everywhere */
  def traverse(tree: Tree)(using Context): Unit =
    if !isUnsafeAssumeSeparate(tree) then trace(i"checking separate $tree"):
      checkUse(tree)
      tree match
        case tree @ Select(qual, _) if tree.symbol.is(Method) && tree.symbol.isConsumeParam =>
          traverseChildren(tree)
          checkConsumedRefs(
              spanCaptures(qual).directFootprint.nonPeaks, qual.nuType,
              TypeRole.Qualifier(qual, tree.symbol),
              i"call prefix of consume ${tree.symbol} refers to", qual.srcPos)
        case tree: GenericApply =>
          traverseChildren(tree)
          tree.tpe match
            case _: MethodOrPoly =>
            case _ => traverseApply(tree)
        case tree: Assign =>
          traverseChildren(tree)
          checkAssign(tree)
        case _: Block | _: Template =>
          traverseSection(tree)
        case tree: ValDef =>
          traverseChildren(tree)
          if !tree.name.is(WildcardParamName) then
            checkValOrDefDef(tree)
        case tree: DefDef =>
          if skippable(tree.symbol) then
            capt.println(i"skipping sep check of ${tree.symbol}")
          else
            inSection:
              consumed.segment:
                for params <- tree.paramss; case param: ValDef <- params do
                  pushDef(param, emptyRefs)
                traverseChildren(tree)
          checkValOrDefDef(tree)
        case If(cond, thenp, elsep) =>
          traverse(cond)
          val thenConsumed = consumed.segment(traverse(thenp))
          val elseConsumed = consumed.segment(traverse(elsep))
          consumed ++= thenConsumed
          consumed ++= elseConsumed
        case tree @ Labeled(bind, expr) =>
          val consumedBuf = mutable.ListBuffer[ConsumedSet]()
          openLabeled = (bind.name, consumedBuf) :: openLabeled
          traverse(expr)
          for cs <- consumedBuf do consumed ++= cs
          openLabeled = openLabeled.tail
        case Return(expr, from) =>
          val retConsumed = consumed.segment(traverse(expr))
          from match
            case Ident(name) =>
              for (lbl, consumedBuf) <- openLabeled do
                if lbl == name then
                  consumedBuf += retConsumed
            case _ =>
        case Match(sel, cases) =>
          // Matches without returns might still be kept after pattern matching to
          // encode table switches.
          traverse(sel)
          val caseConsumed = for cas <- cases yield consumed.segment(traverse(cas))
          caseConsumed.foreach(consumed ++= _)
        case tree: TypeDef if tree.symbol.isClass =>
          consumed.segment:
            traverseChildren(tree)
        case tree: WhileDo =>
          val loopConsumed = consumed.segment(traverseChildren(tree))
          if loopConsumed.size != 0 then
            val (ref, loc) = loopConsumed.toMap.head
            consumeInLoopError(ref, loc)
        case _ =>
          traverseChildren(tree)
end SepCheck
