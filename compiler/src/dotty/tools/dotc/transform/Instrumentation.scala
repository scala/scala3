package dotty.tools
package dotc
package transform

import core._
import Contexts._
import Symbols._
import Flags._
import SymDenotations._

import Decorators._
import ast.Trees._
import MegaPhase._
import StdNames.nme
import Names._
import Constants.Constant


/** The phase is enabled if the -Yinstrument option is set.
 *  If enabled, it counts the number of closures or allocations for each source position.
 *  It does this by generating a call to dotty.tools.dotc.util.Stats.doRecord.
 */
class Instrumentation extends MiniPhase { thisPhase =>
  import ast.tpd._

  override def phaseName: String = "instrumentation"

  override def isEnabled(using Context) =
    ctx.settings.Yinstrument.value

  private val collectionNamesOfInterest = List(
    "toList", "reversedToList"
    /*
    "map", "flatMap", "filter", "filterNot", "withFilter", "collect", "flatten", "foldLeft", "foldRight", "take",
    "reverse", "zip", "++", ":::", ":+", "distinct", "dropRight", "takeRight", "groupBy", "groupMap", "init", "inits",
    "interect", "mkString", "partition", "reverse_:::", "scanLeft", "scanRight",
    "sortBy", "sortWith", "sorted", "span", "splitAt", "takeWhile", "transpose", "unzip", "unzip3",
    "updated", "zipAll", "zipWithIndex",
    "mapConserve", "mapConserve", "filter", "zipWithConserve", "mapWithIndexConserve"*/
  )

  private val listNamesOfInterest = List(
    "extension_::", "extension_tail", "extension_drop", "extension_take",
    "extension_drop", "extension_dropWhile", "unapply")

  private val namesOfInterest = List(
    "::", "+=", "toString", "newArray", "box", "toCharArray", "termName", "typeName",
    "slice", "staticRef", "requiredClass")

  private var namesToRecord: Set[Name] = _
  private var collectionNamesToRecord: Set[Name] = _
  private var listNamesToRecord: Set[Name] = _
  private var Stats_doRecord: Symbol = _
  private var Stats_doRecordSize: Symbol = _
  private var Stats_doRecordListSize: Symbol = _
  private var Stats_doRecordBufferSize: Symbol = _
  private var CollectionIterableClass: ClassSymbol = _
  private var ListBufferClass: ClassSymbol = _

  override def prepareForUnit(tree: Tree)(using Context): Context =
    collectionNamesToRecord = collectionNamesOfInterest.map(_.toTermName).toSet
    listNamesToRecord = listNamesOfInterest.map(_.toTermName).toSet
    namesToRecord = namesOfInterest.map(_.toTermName).toSet ++ listNamesToRecord ++ collectionNamesToRecord
    val StatsModule = requiredModule("dotty.tools.dotc.util.Stats")
    Stats_doRecord = StatsModule.requiredMethod("doRecord")
    Stats_doRecordSize = StatsModule.requiredMethod("doRecordSize")
    Stats_doRecordListSize = StatsModule.requiredMethod("doRecordListSize")
    Stats_doRecordBufferSize = StatsModule.requiredMethod("doRecordBufferSize")
    CollectionIterableClass = requiredClass("scala.collection.Iterable")
    ListBufferClass = requiredClass("dotty.tools.List.Buffer")
    ctx

  private def record(category: String, tree: Tree)(using Context): Tree = {
    val key = Literal(Constant(s"$category@${tree.sourcePos.show}"))
    ref(Stats_doRecord).appliedTo(key, Literal(Constant(1)))
  }

  private def recordSize(tree: Apply)(using Context): Tree = tree.fun match
    case sel @ Select(qual, name)
    if collectionNamesToRecord.contains(name) =>
      val qualType = qual.tpe.widen
      def tryRecord(cls: ClassSymbol, recorder: Symbol): Tree =
        if qualType.derivesFrom(cls) then
          //println(i"record: $tree")
          val key = Literal(Constant(s"totalSize/${name} in ${qualType.classSymbol.name}@${tree.sourcePos.show}"))
          val qual1 = ref(recorder).appliedTo(key, qual).cast(qual.tpe.widen)
          cpy.Apply(tree)(cpy.Select(sel)(qual1, name), tree.args)
        else EmptyTree
      tryRecord(CollectionIterableClass, Stats_doRecordSize)
      .orElse(tryRecord(ListBufferClass, Stats_doRecordBufferSize))
      .orElse(tree)
    case id: RefTree
    if listNamesToRecord.contains(id.name) && tree.args.nonEmpty =>
      val key = Literal(Constant(s"listSize/${id.name}@${tree.sourcePos.show}"))
      val arg1 = ref(Stats_doRecordListSize).appliedTo(key, tree.args.head).cast(tree.args.head.tpe.widen)
      cpy.Apply(tree)(id, arg1 :: tree.args.tail)
    case _ =>
      tree

  private def ok(using Context) =
    !ctx.owner.ownersIterator.exists(_.name.toString.startsWith("Stats"))

  override def transformDefDef(tree: DefDef)(using Context): Tree =
    val sym = tree.symbol
    if ctx.settings.YinstrumentDefs.value
      && ok
      && sym.exists
      && !sym.isOneOf(Synthetic | Artifact)
    then
      def icall = record(i"method/${sym.fullName}", tree)
      def rhs1 = tree.rhs match
        case rhs @ Block(stats, expr) => cpy.Block(rhs)(icall :: stats, expr)
        case _: Match | _: If | _: Try | _: Labeled => cpy.Block(tree.rhs)(icall :: Nil, tree.rhs)
        case rhs => rhs
      cpy.DefDef(tree)(rhs = rhs1)
    else tree

  override def transformApply(tree: Apply)(using Context): Tree = tree.fun match {
    case Select(nu: New, _) =>
      cpy.Block(tree)(record(i"alloc/${nu.tpe}", tree) :: Nil, tree)
    case ref: RefTree if namesToRecord.contains(ref.name) && ok =>
      cpy.Block(tree)(record(i"call/${ref.name}", tree) :: Nil, recordSize(tree))
    case _ =>
      tree
  }

  override def transformBlock(tree: Block)(using Context): Block = tree.expr match {
    case _: Closure =>
      cpy.Block(tree)(record("closure/", tree) :: tree.stats, tree.expr)
    case _ =>
      tree
  }
}
