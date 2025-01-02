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

  extension (cs: CaptureSet)
    def footprint(using Context): CaptureSet =
      def recur(elems: CaptureSet.Refs, newElems: List[CaptureRef]): CaptureSet.Refs = newElems match
        case newElem :: newElems1 =>
          val superElems = newElem.captureSetOfInfo.elems.filter: superElem =>
            !superElem.isMaxCapability && !elems.contains(superElem)
          recur(superElems ++ elems, superElems.toList ++ newElems1)
        case Nil => elems
      val elems: CaptureSet.Refs = cs.elems.filter(!_.isMaxCapability)
      CaptureSet(recur(elems, elems.toList))

    def overlapWith(other: CaptureSet)(using Context): CaptureSet.Refs =
      val refs1 = cs.elems
      val refs2 = other.elems
      def common(refs1: CaptureSet.Refs, refs2: CaptureSet.Refs) =
        refs1.filter: ref =>
          ref.isExclusive && refs2.exists(_.stripReadOnly eq ref)
      common(refs1, refs2) ++ common(refs2, refs1)

  private def hidden(elem: CaptureRef)(using Context): CaptureSet.Refs = elem match
    case Fresh.Cap(hcs) => hcs.elems.filter(!_.isRootCapability) ++ hidden(hcs)
    case ReadOnlyCapability(ref) => hidden(ref).map(_.readOnly)
    case _ => emptySet

  private def hidden(cs: CaptureSet)(using Context): CaptureSet.Refs =
    val seen: util.EqHashSet[CaptureRef] = new util.EqHashSet

    def hiddenByElem(elem: CaptureRef): CaptureSet.Refs =
      if seen.add(elem) then elem match
        case Fresh.Cap(hcs) => hcs.elems.filter(!_.isRootCapability) ++ recur(hcs)
        case ReadOnlyCapability(ref) => hiddenByElem(ref).map(_.readOnly)
        case _ => emptySet
      else emptySet

    def recur(cs: CaptureSet): CaptureSet.Refs =
      (emptySet /: cs.elems): (elems, elem) =>
        elems ++ hiddenByElem(elem)

    recur(cs)
  end hidden

  private def checkApply(fn: Tree, args: List[Tree])(using Context): Unit =
    val fnCaptures = fn.nuType.deepCaptureSet

    def captures(arg: Tree) =
      val argType = arg.nuType
      if argType.hasUseAnnot then argType.deepCaptureSet else argType.captureSet

    val argCaptures = args.map(captures)
    capt.println(i"check separate $fn($args), fnCaptures = $fnCaptures, argCaptures = $argCaptures")
    var footprint = args.foldLeft(fnCaptures.footprint): (fp, arg) =>
      if arg.needsSepCheck then fp else fp ++ captures(arg)
    val paramNames = fn.nuType.widen match
      case MethodType(pnames) => pnames
      case _ => args.indices.map(nme.syntheticParamName(_))
    for (arg, pname) <- args.lazyZip(paramNames) do
      if arg.needsSepCheck then
        val ac = captures(arg)
        val hiddenInArg = CaptureSet(hidden(ac)).footprint
        //println(i"check sep $arg: $ac, footprint so far = $footprint, hidden = $hiddenInArg")
        val overlap = hiddenInArg.footprint.overlapWith(footprint)
        if !overlap.isEmpty then
          def formalName = if pname.toString.contains('$') then "" else i"$pname "
          def whatStr = if overlap.size == 1 then "this capability is" else "these capabilities are"
          def funStr =
            if fn.symbol.exists then i"${fn.symbol}"
            else "the function"
          report.error(
            em"""Separation failure: argument of type ${arg.actualType} to capture-polymorphic parameter
                |${formalName}of type ${arg.nuType} captures ${CaptureSet(overlap)}, and $whatStr also passed separately to $funStr.""",
            arg.srcPos)
        footprint ++= hiddenInArg.footprint
        footprint ++= ac.footprint

  private def traverseApply(tree: Tree, argss: List[List[Tree]])(using Context): Unit = tree match
    case Apply(fn, args) => traverseApply(fn, args :: argss)
    case TypeApply(fn, args) => traverseApply(fn, argss) // skip type arguments
    case _ =>
      if argss.nestedExists(_.needsSepCheck) then
        checkApply(tree, argss.flatten)

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






