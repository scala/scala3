package dotty.tools
package dotc
package cc
import ast.tpd
import collection.mutable

import core.*
import Symbols.*, Types.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import CaptureSet.{Refs, emptySet}
import config.Printers.capt
import StdNames.nme

class SepChecker(checker: CheckCaptures.CheckerAPI) extends tpd.TreeTraverser:
  import tpd.*
  import checker.*

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

  private def hidden(refs: Refs)(using Context): Refs =
    val seen: util.EqHashSet[CaptureRef] = new util.EqHashSet

    def hiddenByElem(elem: CaptureRef): Refs =
      if seen.add(elem) then elem match
        case Fresh.Cap(hcs) => hcs.elems.filter(!_.isRootCapability) ++ recur(hcs.elems)
        case ReadOnlyCapability(ref) => hiddenByElem(ref).map(_.readOnly)
        case _ => emptySet
      else emptySet

    def recur(cs: Refs): Refs =
      (emptySet /: cs): (elems, elem) =>
        elems ++ hiddenByElem(elem)

    recur(refs)
  end hidden

  /** The captures of an argument or prefix widened to the formal parameter, if
   *  the latter contains a cap.
   */
  private def formalCaptures(arg: Tree)(using Context): Refs =
    val argType = arg.formalType.orElse(arg.nuType)
    (if arg.nuType.hasUseAnnot then argType.deepCaptureSet else argType.captureSet)
      .elems

   /** The captures of an argument of prefix. No widening takes place */
  private def actualCaptures(arg: Tree)(using Context): Refs =
    val argType = arg.nuType
    (if argType.hasUseAnnot then argType.deepCaptureSet else argType.captureSet)
      .elems

  private def sepError(fn: Tree, args: List[Tree], argIdx: Int,
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
    def funStr =
      if fn.symbol.exists then i"${fn.symbol}: ${fn.symbol.info}"
      else i"a function of type ${fn.nuType.widen}"
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
      if clashIdx == 0 then methPart(fn).asInstanceOf[Select].qualifier
      else args(clashIdx - 1)
    def clashType = clashTree.nuType
    def clashCaptures = actualCaptures(clashTree)
    def hiddenCaptures = hidden(formalCaptures(arg))
    def clashFootprint = clashCaptures.footprint
    def hiddenFootprint = hiddenCaptures.footprint
    def declaredFootprint = deps(arg).map(actualCaptures(_)).foldLeft(emptySet)(_ ++ _).footprint
    def footprintOverlap = hiddenFootprint.overlapWith(clashFootprint) -- declaredFootprint
    report.error(
      em"""Separation failure: argument of type  ${arg.nuType}
          |to $funStr
          |corresponds to capture-polymorphic formal parameter ${formalName}of type  ${arg.formalType}
          |and captures ${CaptureSet(overlap)}, but $whatStr also passed separately
          |in the ${whereStr.trim} with type  $clashType.
          |
          |  Capture set of $whereStr        : ${CaptureSet(clashCaptures)}
          |  Hidden set of current argument        : ${CaptureSet(hiddenCaptures)}
          |  Footprint of $whereStr          : ${CaptureSet(clashFootprint)}
          |  Hidden footprint of current argument  : ${CaptureSet(hiddenFootprint)}
          |  Declared footprint of current argument: ${CaptureSet(declaredFootprint)}
          |  Undeclared overlap of footprints      : ${CaptureSet(footprintOverlap)}""",
      arg.srcPos)
  end sepError

  private def checkApply(fn: Tree, args: List[Tree], deps: collection.Map[Tree, List[Tree]])(using Context): Unit =
    val fnCaptures = methPart(fn) match
      case Select(qual, _) => qual.nuType.captureSet
      case _ => CaptureSet.empty
    capt.println(i"check separate $fn($args), fnCaptures = $fnCaptures, argCaptures = ${args.map(arg => CaptureSet(formalCaptures(arg)))}, deps = ${deps.toList}")
    var footprint = fnCaptures.elems.footprint
    val footprints = mutable.ListBuffer[(Refs, Int)]((footprint, 0))
    val indexedArgs = args.zipWithIndex

    def subtractDeps(elems: Refs, arg: Tree): Refs =
      deps(arg).foldLeft(elems): (elems, dep) =>
        elems -- actualCaptures(dep).footprint

    for (arg, idx) <- indexedArgs do
      if !arg.needsSepCheck then
        footprint = footprint ++ subtractDeps(actualCaptures(arg).footprint, arg)
        footprints += ((footprint, idx + 1))
    for (arg, idx) <- indexedArgs do
      if arg.needsSepCheck then
        val ac = formalCaptures(arg)
        val hiddenInArg = hidden(ac).footprint
        //println(i"check sep $arg: $ac, footprint so far = $footprint, hidden = $hiddenInArg")
        val overlap = subtractDeps(hiddenInArg.overlapWith(footprint), arg)
        if !overlap.isEmpty then
          sepError(fn, args, idx, overlap, hiddenInArg, footprints.toList, deps)
        footprint ++= actualCaptures(arg).footprint
        footprints += ((footprint, idx + 1))
  end checkApply

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
      val referred = dep match
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
      case tree: GenericApply =>
        if tree.symbol != defn.Caps_unsafeAssumeSeparate then
          tree.tpe match
            case _: MethodOrPoly =>
            case _ => traverseApply(tree, Nil)
          traverseChildren(tree)
      case _ =>
        traverseChildren(tree)
end SepChecker






