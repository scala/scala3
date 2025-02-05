package dotty.tools
package dotc
package cc
import ast.tpd
import collection.mutable

import core.*
import Symbols.*, Types.*, Flags.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import CaptureSet.{Refs, emptyRefs, HiddenSet}
import config.Printers.capt
import StdNames.nme
import util.{SimpleIdentitySet, EqHashMap, SrcPos}
import tpd.*
import reflect.ClassTag

/** The separation checker is  a tree traverser that is run after capture checking.
 *  It checks tree nodes for various separation conditions, explained in the
 *  methods below. Rough summary:
 *
 *   - Hidden sets of arguments must not be referred to in the same application
 *   - Hidden sets of (result-) types must not be referred to alter in the same scope.
 *   - Returned hidden sets can only refer to @consume parameters.
 *   - If returned hidden sets refer to an encloding this, the reference must be
 *     from a @consume method.
 *   - Consumed entities cannot be used subsequently.
 *   - Entitites cannot be consumed in a loop.
 */
object SepCheck:

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

  /** The role in which a checked type appears, used for composing error messages */
  enum TypeRole:
    case Result(sym: Symbol, inferred: Boolean)
    case Argument(arg: Tree)
    case Qualifier(qual: Tree, meth: Symbol)

    /** If this is a Result tole, the associated symbol, otherwise NoSymbol */
    def dclSym = this match
      case Result(sym, _) => sym
      case _ => NoSymbol

    /** A textual description of this role */
    def description(using Context): String = this match
      case Result(sym, inferred) =>
        def inferredStr = if inferred then " inferred" else ""
        def resultStr = if sym.info.isInstanceOf[MethodicType] then " result" else ""
        i"$sym's$inferredStr$resultStr type"
      case TypeRole.Argument(_) =>
        "the argument's adapted type"
      case TypeRole.Qualifier(_, meth) =>
        i"the type of the prefix to a call of $meth"
  end TypeRole

  /** A class for segmented sets of consumed references.
   *  References are associated with the source positions where they first appeared.
   *  References are compared with `eq`.
   */
  abstract class ConsumedSet:
    /** The references in the set. The array should be treated as immutable in client code */
    def refs: Array[CaptureRef]

    /** The associated source positoons. The array should be treated as immutable in client code */
    def locs: Array[SrcPos]

    /** The number of references in the set */
    def size: Int

    def toMap: Map[CaptureRef, SrcPos] = refs.take(size).zip(locs).toMap

    def show(using Context) =
      s"[${toMap.map((ref, loc) => i"$ref -> $loc").toList}]"
  end ConsumedSet

  /** A fixed consumed set consisting of the given references `refs` and
   *  associated source positions `locs`
   */
  class ConstConsumedSet(val refs: Array[CaptureRef], val locs: Array[SrcPos]) extends ConsumedSet:
    def size = refs.size

  /** A mutable consumed set, which is initially empty */
  class MutConsumedSet extends ConsumedSet:
    var refs: Array[CaptureRef] = new Array(4)
    var locs: Array[SrcPos] = new Array(4)
    var size = 0

    private def double[T <: AnyRef : ClassTag](xs: Array[T]): Array[T] =
      val xs1 = new Array[T](xs.length * 2)
      xs.copyToArray(xs1)
      xs1

    private def ensureCapacity(added: Int): Unit =
      if size + added > refs.length then
        refs = double(refs)
        locs = double(locs)

    /** If `ref` is in the set, its associated source position, otherwise `null` */
    def get(ref: CaptureRef): SrcPos | Null =
      var i = 0
      while i < size && (refs(i) ne ref) do i += 1
      if i < size then locs(i) else null

    /** If `ref` is not yet in the set, add it with given source position */
    def put(ref: CaptureRef, loc: SrcPos): Unit =
      if get(ref) == null then
        ensureCapacity(1)
        refs(size) = ref
        locs(size) = loc
        size += 1

    /** Add all references with their associated positions from `that` which
     *  are not yet in the set.
     */
    def ++= (that: ConsumedSet): Unit =
      for i <- 0 until that.size do put(that.refs(i), that.locs(i))

    /** Run `op` and return any new references it created in a separate `ConsumedSet`.
     *  The current mutable set is reset to its state before `op` was run.
     */
    def segment(op: => Unit): ConsumedSet =
      val start = size
      try
        op
        if size == start then EmptyConsumedSet
        else ConstConsumedSet(refs.slice(start, size), locs.slice(start, size))
      finally
        size = start
  end MutConsumedSet

  val EmptyConsumedSet = ConstConsumedSet(Array(), Array())

class SepCheck(checker: CheckCaptures.CheckerAPI) extends tpd.TreeTraverser:
  import checker.*
  import SepCheck.*

  /** The set of capabilities that are hidden by a polymorphic result type
   *  of some previous definition.
   */
  private var defsShadow: Refs = emptyRefs

  /** A map from definitions to their internal result types.
   *  Populated during separation checking traversal.
   */
  private val resultType = EqHashMap[Symbol, Type]()

  /** The previous val or def definitions encountered during separation checking.
   *  These all enclose and precede the current traversal node.
   */
  private var previousDefs: List[mutable.ListBuffer[ValOrDefDef]] = Nil

  /** The set of references that were consumed so far in the current method */
  private var consumed: MutConsumedSet = MutConsumedSet()

  /** Run `op`` with a fresh, initially empty consumed set. */
  private def withFreshConsumed(op: => Unit): Unit =
    val saved = consumed
    consumed = MutConsumedSet()
    op
    consumed = saved

  /** Infos aboput Labeled expressions enclosing the current traversal point.
   *  For each labeled expression, it's label name, and a list buffer containing
   *  all consumed sets of return expressions referring to that label.
   */
  private var openLabeled: List[(Name, mutable.ListBuffer[ConsumedSet])] = Nil

  extension (refs: Refs)

    /** The footprint of a set of references `refs` the smallest set `F` such that
     *   - no maximal capability is in `F`
     *   - all non-maximal capabilities in `refs` are in `F`
     *   - if `f in F` then the footprint of `f`'s info is also in `F`.
     */
    private def footprint(using Context): Refs =
      def recur(elems: Refs, newElems: List[CaptureRef]): Refs = newElems match
        case newElem :: newElems1 =>
          val superElems = newElem.captureSetOfInfo.elems.filter: superElem =>
            !superElem.isMaxCapability && !elems.contains(superElem)
          recur(elems ++ superElems, newElems1 ++ superElems.toList)
        case Nil => elems
      val elems: Refs = refs.filter(!_.isMaxCapability)
      recur(elems, elems.toList)

    /** The overlap of two footprint sets F1 and F2. This contains all exclusive references `r`
     *  such that one of the following is true:
     *   1.
     *      - one of the sets contains `r`
     *      - the other contains a capability `s` or `s.rd` where `s` _covers_ `r`
     *   2.
     *      - one of the sets contains `r.rd`
     *      - the other contains a capability `s` where `s` _covers_ `r`
     *
     *  A capability `s` covers `r` if `r` can be seen as a path extension of `s`. E.g.
     *  if `s = x.a` and `r = x.a.b.c` then `s` covers `a`.
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
          ref.isExclusive && refs2.exists(_.stripReadOnly.covers(ref))
        ++
        refs1
          .filter:
            case ReadOnlyCapability(ref @ TermRef(prefix: CaptureRef, _)) =>
              // We can get away testing only references with at least one field selection
              // here since stripped readOnly references that equal a reference in refs2
              // are added by the first clause of the symmetric call to common.
              !ref.isCap && refs2.exists(_.covers(prefix))
            case _ =>
              false
          .map(_.stripReadOnly)

      common(refs, other) ++ common(other, refs)
    end overlapWith

    /** The non-maximal elements hidden directly or indirectly by a maximal
     *  capability in `refs`. E g. if `R = {x, <cap hiding <y, <cap hiding z>>}` then
     *  its hidden set is `{y, z}`.
     */
    private def hidden(using Context): Refs =
      val seen: util.EqHashSet[CaptureRef] = new util.EqHashSet

      def hiddenByElem(elem: CaptureRef): Refs = elem match
        case Fresh.Cap(hcs) => hcs.elems.filter(!_.isRootCapability) ++ recur(hcs.elems)
        case ReadOnlyCapability(ref1) => hiddenByElem(ref1).map(_.readOnly)
        case _ => emptyRefs

      def recur(refs: Refs): Refs =
        (emptyRefs /: refs): (elems, elem) =>
          if seen.add(elem) then elems ++ hiddenByElem(elem) else elems

      recur(refs)
    end hidden

    /** Same as !refs.hidden.isEmpty but more efficient */
    private def containsHidden(using Context): Boolean =
      val seen: util.EqHashSet[CaptureRef] = new util.EqHashSet

      def recur(refs: Refs): Boolean = refs.exists: ref =>
        seen.add(ref) && ref.stripReadOnly.match
          case Fresh.Cap(hcs) =>
            hcs.elems.exists(!_.isRootCapability) || recur(hcs.elems)
          case _ =>
            false

      recur(refs)
    end containsHidden

    /** Subtract all elements that are covered by some element in `others` from this set. */
    private def deduct(others: Refs)(using Context): Refs =
      refs.filter: ref =>
        !others.exists(_.covers(ref))

    /** Deduct the footprint of `sym` and `sym*` from `refs` */
    private def deductSym(sym: Symbol)(using Context): Refs =
      val ref = sym.termRef
      if ref.isTrackableRef then refs.deduct(CaptureSet(ref, ref.reach).elems.footprint)
      else refs

    /** Deduct the footprint of all captures of trees in `deps` from `refs` */
    private def deductCapturesOf(deps: List[Tree])(using Context): Refs =
      deps.foldLeft(refs): (refs, dep) =>
        refs.deduct(captures(dep).footprint)
  end extension

  /** The deep capture set of an argument or prefix widened to the formal parameter, if
   *  the latter contains a cap.
   */
  private def formalCaptures(arg: Tree)(using Context): Refs =
    arg.formalType.orElse(arg.nuType).deepCaptureSet.elems

   /** The deep capture set if the type of `tree` */
  private def captures(tree: Tree)(using Context): Refs =
   tree.nuType.deepCaptureSet.elems

  // ---- Error reporting TODO Once these are stabilized, move to messages -----

  /** Report a separation failure in an application `fn(args)`
   *  @param fn          the function
   *  @param args        the flattened argument lists
   *  @param argIdx      the index of the failing argument in `args`, starting at 0
   *  @param overlap     the overlap causing the failure
   *  @param hiddenInArg the hidxden set of the type of the failing argument
   *  @param footprints  a sequence of partial footprints, and the index of the
   *                     last argument they cover.
   *  @param deps        cross argument dependencies: maps argument trees to
   *                     those other arguments that where mentioned by coorresponding
   *                     formal parameters.
   */
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
    def declaredFootprint = deps(arg).map(captures(_)).foldLeft(emptyRefs)(_ ++ _).footprint
    def footprintOverlap = hiddenFootprint.overlapWith(clashFootprint).deduct(declaredFootprint)
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

  /** Report a use/definition failure, where a previously hidden capability is
   *  used again.
   *  @param tree          the tree where the capability is used
   *  @param used          the footprint of all uses of `tree`
   *  @param globalOverlap the overlap between `used` and all capabilities hidden
   *                       by previous definitions
   */
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

  /** Report a failure where a previously consumed capability is used again,
   *  @param ref     the capability that is used after being consumed
   *  @param loc     the position where the capability was consumed
   *  @param pos     the position where the capability was used again
   */
  def consumeError(ref: CaptureRef, loc: SrcPos, pos: SrcPos)(using Context): Unit =
    report.error(
      em"""Separation failure: Illegal access to $ref, which was passed to a
          |@consume parameter or was used as a prefix to a @consume method on line ${loc.line + 1}
          |and therefore is no longer available.""",
      pos)

  /** Report a failure where a capability is consumed in a loop.
   *  @param ref     the capability
   *  @param loc     the position where the capability was consumed
   */
  def consumeInLoopError(ref: CaptureRef, pos: SrcPos)(using Context): Unit =
    report.error(
      em"""Separation failure: $ref appears in a loop, therefore it cannot
          |be passed to a @consume parameter or be used as a prefix of a @consume method call.""",
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
   *    def seq(x: () => Unit; y ->{cap, x} Unit): Unit
   *    def f: () ->{io} Unit
   *
   *  we do allow `seq(f, f)` even though `{f, io}` is in the hidden set of the
   *  first parameter `x`, since the second parameter explicitly mentions `x` in
   *  its capture set.
   *
   *  Also check separation via checkType within individual arguments widened to their
   *  formal paramater types.
   *
   *  @param fn        the applied function
   *  @param args      the flattened argument lists
   *  @param deps      cross argument dependencies: maps argument trees to
   *                   those other arguments that where mentioned by coorresponding
   *                   formal parameters.
   */
  private def checkApply(fn: Tree, args: List[Tree], deps: collection.Map[Tree, List[Tree]])(using Context): Unit =
    val fnCaptures = methPart(fn) match
      case Select(qual, _) => qual.nuType.captureSet
      case _ => CaptureSet.empty
    capt.println(i"check separate $fn($args), fnCaptures = $fnCaptures, argCaptures = ${args.map(arg => CaptureSet(formalCaptures(arg)))}, deps = ${deps.toList}")
    var footprint = fnCaptures.elems.footprint
    val footprints = mutable.ListBuffer[(Refs, Int)]((footprint, 0))
    val indexedArgs = args.zipWithIndex

    // First, compute all footprints of arguments to monomorphic pararameters,
    // separately in `footprints`, and their union in `footprint`.
    for (arg, idx) <- indexedArgs do
      if !arg.needsSepCheck then
        footprint = footprint ++ captures(arg).footprint.deductCapturesOf(deps(arg))
        footprints += ((footprint, idx + 1))

    // Then, for each argument to a polymorphic parameter:
    //   - check formal type via checkType
    //   - check that hidden set of argument does not overlap with current footprint
    //   - add footprint of the deep capture set of actual type of argument
    //     to global footprint(s)
    for (arg, idx) <- indexedArgs do
      if arg.needsSepCheck then
        val ac = formalCaptures(arg)
        checkType(arg.formalType, arg.srcPos, TypeRole.Argument(arg))
        val hiddenInArg = ac.hidden.footprint
        //println(i"check sep $arg: $ac, footprint so far = $footprint, hidden = $hiddenInArg")
        val overlap = hiddenInArg.overlapWith(footprint).deductCapturesOf(deps(arg))
        if !overlap.isEmpty then
          sepApplyError(fn, args, idx, overlap, hiddenInArg, footprints.toList, deps)
        footprint ++= captures(arg).footprint
        footprints += ((footprint, idx + 1))
  end checkApply

  /** The def/use overlap between the references `hiddenByDef` hidden by
   *  a previous definition and the `used` set of a tree with symbol `sym`.
   *  Deduct any capabilities referred to or hidden by the (result-) type of `sym`.
   */
  def defUseOverlap(hiddenByDef: Refs, used: Refs, sym: Symbol)(using Context): Refs =
    val overlap = hiddenByDef.overlapWith(used)
    resultType.get(sym) match
      case Some(tp) if !overlap.isEmpty =>
        val declared = tp.captureSet.elems
        overlap.deduct(declared.footprint).deduct(declared.hidden.footprint)
      case _ =>
        overlap

  /** 1. Check that the capabilities used at `tree` don't overlap with
   *     capabilities hidden by a previous definition.
   *  2. Also check that none of the used capabilities was consumed before.
   */
  def checkUse(tree: Tree)(using Context) =
    val used = tree.markedFree
    if !used.elems.isEmpty then
      val usedFootprint = used.elems.footprint
      val overlap = defUseOverlap(defsShadow, usedFootprint, tree.symbol)
      if !overlap.isEmpty then
        sepUseError(tree, usedFootprint, overlap)
      for ref <- used.elems do
        val pos = consumed.get(ref)
        if pos != null then consumeError(ref, pos, tree.srcPos)

  /** If `tp` denotes some version of a singleton type `x.type` the set `{x}`
   *  otherwise the empty set.
   */
  def explicitRefs(tp: Type): Refs = tp match
    case tp: (TermRef | ThisType) => SimpleIdentitySet(tp)
    case AnnotatedType(parent, _) => explicitRefs(parent)
    case AndType(tp1, tp2) => explicitRefs(tp1) ++ explicitRefs(tp2)
    case OrType(tp1, tp2) => explicitRefs(tp1) ** explicitRefs(tp2)
    case _ => emptyRefs

  /** Deduct some elements from `refs` according to the role of the checked type `tpe`:
   *   - If the the type apears as a (result-) type of a definition of `x`, deduct
   *     `x` and `x*`.
   *   - If `tpe` is morally a singleton type deduct it as well.
   */
  def prune(refs: Refs, tpe: Type, role: TypeRole)(using Context): Refs =
    refs.deductSym(role.dclSym).deduct(explicitRefs(tpe))

  /** Check validity of consumed references `refsToCheck`. The references are consumed
   *  because they are hidden in a Fresh.Cap result type or they are referred
   *  to in an argument to a @consume parameter or in a prefix of a @consume method --
   *  which one applies is determined by the role parameter.
   *
   *  This entails the following checks:
   *   - The reference must be defined in the same as method or class as
   *     the access.
   *   - If the reference is to a term parameter, that parameter must be
   *     marked as @consume as well.
   *   - If the reference is to a this type of the enclosing class, the
   *     access must be in a @consume method.
   *
   *  References that extend SharedCapability are excluded from checking.
   *  As a side effect, add all checked references with the given position `pos`
   *  to the global `consumed` map.
   *
   *  @param refsToCheck   the referencves to check
   *  @param tpe           the type containing those references
   *  @param role          the role in which the type apears
   *  @param descr         a textual description of the type and its relationship with the checked reference
   *  @param pos           position for error reporting
   */
  def checkConsumedRefs(refsToCheck: Refs, tpe: Type, role: TypeRole, descr: => String, pos: SrcPos)(using Context) =
    val badParams = mutable.ListBuffer[Symbol]()
    def currentOwner = role.dclSym.orElse(ctx.owner)
    for hiddenRef <- prune(refsToCheck, tpe, role) do
      val proot = hiddenRef.pathRootOrShared
      if !proot.widen.derivesFromSharedCapability then
        proot match
          case ref: TermRef =>
            val refSym = ref.symbol
            if currentOwner.enclosingMethodOrClass.isProperlyContainedIn(refSym.maybeOwner.enclosingMethodOrClass) then
              report.error(em"""Separation failure: $descr non-local $refSym""", pos)
            else if refSym.is(TermParam)
              && !refSym.hasAnnotation(defn.ConsumeAnnot)
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
                    |The access must be in a @consume method to allow this.""",
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
            |The parameter$pluralS need$singleS to be annotated with @consume to allow this.""",
          pos)

    role match
      case _: TypeRole.Argument | _: TypeRole.Qualifier =>
        for ref <- refsToCheck do
          if !ref.pathRootOrShared.derivesFromSharedCapability then
            consumed.put(ref, pos)
      case _ =>
  end checkConsumedRefs

  /** Check separation conditions of type `tpe` that appears in `role`.
   *   1. Check that the parts of type `tpe` are mutually separated, as defined in
   *      `checkParts` below.
   *   2. Check that validity of all references consumed by the type as defined in
   *      `checkLegalRefs` below
   */
  def checkType(tpe: Type, pos: SrcPos, role: TypeRole)(using Context): Unit =

    /** Check that the parts of type `tpe` are mutually separated.
     *  This means that references hidden in some part of the type may not
     *  be explicitly referenced or hidden in some other part.
     */
    def checkParts(parts: List[Type]): Unit =
      var footprint: Refs = emptyRefs
      var hiddenSet: Refs = emptyRefs
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
                val prevRefs = prune(mapRefs(prev.deepCaptureSet.elems).footprint, tpe, role)
                (i",  $prev , ", prevRefs, prevRefs.overlapWith(next))
              .dropWhile(_._3.isEmpty)
              .nextOption
              .getOrElse(("", current, globalOverlap))
            report.error(
              em"""Separation failure in ${role.description} $tpe.
                  |One part,  $part , $nextRel  ${CaptureSet(next)}.
                  |A previous part$prevStr $prevRel  ${CaptureSet(prevRefs)}.
                  |The two sets overlap at  ${CaptureSet(overlap)}.""",
              pos)

        val partRefs = part.deepCaptureSet.elems
        val partFootprint = prune(partRefs.footprint, tpe, role)
        val partHidden = prune(partRefs.hidden.footprint, tpe, role).deduct(partFootprint)

        checkSep(footprint, partHidden, identity, "references", "hides")
        checkSep(hiddenSet, partHidden, _.hidden, "also hides", "hides")
        checkSep(hiddenSet, partFootprint, _.hidden, "hides", "references")

        footprint ++= partFootprint
        hiddenSet ++= partHidden
        checked += 1
      end for
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

    /** If `tpe` appears as a (result-) type of a definition, treat its
     *  hidden set minus its explicitly declared footprint as consumed.
     *  If `tpe` appears as an argument to a @consume parameter, treat
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
          val toCheck = refs.hidden.footprint.deduct(refs.footprint)
          checkConsumedRefs(toCheck, tpe, role, i"${role.description} $tpe hides", pos)
      case TypeRole.Argument(arg) =>
        if tpe.hasAnnotation(defn.ConsumeAnnot) then
          val capts = captures(arg).footprint
          checkConsumedRefs(capts, tpe, role, i"argument to @consume parameter with type ${arg.nuType} refers to", pos)
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
   *    f(x: A, y: B^{cap, x}, z: C^{x, y}): D
   *
   *  then the dependencies of an application `f(a, b)` is a map that takes
   *  `b` to `List(a)` and `c` to `List(a, b)`.
   */
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

  /** Decompose an application into a function prefix and a list of argument lists.
   *  If some of the arguments need a separation check because they are capture polymorphic,
   *  perform a separation check with `checkApply`
   */
  private def traverseApply(tree: Tree, argss: List[List[Tree]])(using Context): Unit = tree match
    case Apply(fn, args) => traverseApply(fn, args :: argss)
    case TypeApply(fn, args) => traverseApply(fn, argss) // skip type arguments
    case _ =>
      if argss.nestedExists(_.needsSepCheck) then
        checkApply(tree, argss.flatten, dependencies(tree, argss))

  /** Is `tree` an application of `caps.unsafe.unsafeAssumeSeparate`? */
  def isUnsafeAssumeSeparate(tree: Tree)(using Context): Boolean = tree match
    case tree: Apply => tree.symbol == defn.Caps_unsafeAssumeSeparate
    case _ => false

  /** Check (result-) type of `tree` for separation conditions using `checkType`.
   *  Excluded are parameters and definitions that have an =unsafeAssumeSeparate
   *  application as right hand sides.
   *  Hidden sets of checked definitions are added to `defsShadow`.
   */
  def checkValOrDefDef(tree: ValOrDefDef)(using Context): Unit =
    if !tree.symbol.isOneOf(TermParamOrAccessor) && !isUnsafeAssumeSeparate(tree.rhs) then
      checkType(tree.tpt, tree.symbol)
      if previousDefs.nonEmpty then
        capt.println(i"sep check def ${tree.symbol}: ${tree.tpt} with ${captures(tree.tpt).hidden.footprint}")
        defsShadow ++= captures(tree.tpt).hidden.footprint.deductSym(tree.symbol)
        resultType(tree.symbol) = tree.tpt.nuType
        previousDefs.head += tree

  /** Traverse `tree` and perform separation checks everywhere */
  def traverse(tree: Tree)(using Context): Unit =
    if isUnsafeAssumeSeparate(tree) then return
    checkUse(tree)
    tree match
      case tree @ Select(qual, _) if tree.symbol.is(Method) && tree.symbol.hasAnnotation(defn.ConsumeAnnot) =>
        traverseChildren(tree)
        checkConsumedRefs(
            captures(qual).footprint, qual.nuType,
            TypeRole.Qualifier(qual, tree.symbol),
            i"call prefix of @consume ${tree.symbol} refers to", qual.srcPos)
      case tree: GenericApply =>
        traverseChildren(tree)
        tree.tpe match
          case _: MethodOrPoly =>
          case _ => traverseApply(tree, Nil)
      case tree: Block =>
        val saved = defsShadow
        previousDefs = mutable.ListBuffer() :: previousDefs
        try traverseChildren(tree)
        finally
          previousDefs = previousDefs.tail
          defsShadow = saved
      case tree: ValDef =>
        traverseChildren(tree)
        checkValOrDefDef(tree)
      case tree: DefDef =>
        withFreshConsumed:
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
        withFreshConsumed:
          traverseChildren(tree)
      case tree: WhileDo =>
        val loopConsumed = consumed.segment(traverseChildren(tree))
        if loopConsumed.size != 0 then
          val (ref, pos) = loopConsumed.toMap.head
          consumeInLoopError(ref, pos)
      case _ =>
        traverseChildren(tree)
end SepCheck