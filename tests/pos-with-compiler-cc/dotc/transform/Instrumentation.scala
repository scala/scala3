package dotty.tools
package dotc
package transform

import core._
import Contexts._
import Symbols._
import Flags._

import Decorators._
import MegaPhase._
import Names._
import Constants.Constant


/** The phase is enabled if the -Yinstrument option is set.
 *  If enabled, it counts the number of closures or allocations for each source position.
 *  It does this by generating a call to dotty.tools.dotc.util.Stats.doRecord.
 */
class Instrumentation extends MiniPhase { thisPhase =>
  import ast.tpd._

  override def phaseName: String = Instrumentation.name

  override def description: String = Instrumentation.description

  override def isEnabled(using Context) =
    ctx.settings.Yinstrument.value

  private val collectionNamesOfInterest = List(
    "map", "flatMap", "filter", "filterNot", "withFilter", "collect", "flatten", "foldLeft", "foldRight", "take",
    "reverse", "zip", "++", ":::", ":+", "distinct", "dropRight", "takeRight", "groupBy", "groupMap", "init", "inits",
    "interect", "mkString", "partition", "reverse_:::", "scanLeft", "scanRight",
    "sortBy", "sortWith", "sorted", "span", "splitAt", "takeWhile", "transpose", "unzip", "unzip3",
    "updated", "zipAll", "zipWithIndex",
    "mapConserve", "mapconserve", "filterConserve", "zipWithConserve", "mapWithIndexConserve"
  )

  private val namesOfInterest = collectionNamesOfInterest ++ List(
    "::", "+=", "toString", "newArray", "box", "toCharArray", "termName", "typeName",
    "slice", "staticRef", "requiredClass")

  private var namesToRecord: Set[Name] = _
  private var collectionNamesToRecord: Set[Name] = _
  private var Stats_doRecord: Symbol = _
  private var Stats_doRecordSize: Symbol = _
  private var CollectionIterableClass: ClassSymbol = _

  override def prepareForUnit(tree: Tree)(using Context): Context =
    namesToRecord = namesOfInterest.map(_.toTermName).toSet
    collectionNamesToRecord = collectionNamesOfInterest.map(_.toTermName).toSet
    val StatsModule = requiredModule("dotty.tools.dotc.util.Stats")
    Stats_doRecord = StatsModule.requiredMethod("doRecord")
    Stats_doRecordSize = StatsModule.requiredMethod("doRecordSize")
    CollectionIterableClass = requiredClass("scala.collection.Iterable")
    ctx

  private def record(category: String, tree: Tree)(using Context): Tree = {
    val key = Literal(Constant(s"$category@${tree.sourcePos.show}"))
    ref(Stats_doRecord).appliedTo(key, Literal(Constant(1)))
  }

  private def recordSize(tree: Apply)(using Context): Tree = tree.fun match
    case sel @ Select(qual, name)
    if collectionNamesToRecord.contains(name)
       && qual.tpe.widen.derivesFrom(CollectionIterableClass) =>
      val key = Literal(Constant(s"totalSize/${name} in ${qual.tpe.widen.classSymbol.name}@${tree.sourcePos.show}"))
      val qual1 = ref(Stats_doRecordSize).appliedTo(key, qual).cast(qual.tpe.widen)
      cpy.Apply(tree)(cpy.Select(sel)(qual1, name), tree.args)
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

object Instrumentation:
  val name: String = "instrumentation"
  val description: String = "count calls and allocations under -Yinstrument"
