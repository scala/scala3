package dotty.tools
package dotc
package reporting

import core.*
import Contexts.{Context, ctx}
import Symbols.{Symbol, NoSymbol}
import collection.mutable
import util.{EqHashMap, NoSourcePosition}
import util.Spans.{Span, NoSpan}
import Decorators.i
import parsing.Scanners.Scanner
import io.AbstractFile
import annotation.internal.sharable

abstract class Profile:
  def unitProfile(unit: CompilationUnit): Profile.Info
  def recordNewLine()(using Context): Unit
  def recordNewToken()(using Context): Unit
  def recordTasty(size: Int)(using Context): Unit
  def recordMethodSize(meth: Symbol, size: Int, span: Span)(using Context): Unit
  def printSummary()(using Context): Unit

object Profile:
  def current(using Context): Profile =
    val run = ctx.run
    if run == null then NoProfile else run.profile

  inline val TastyChunkSize = 50

  case class MethodInfo(meth: Symbol, size: Int, span: Span)
  @sharable object NoInfo extends MethodInfo(NoSymbol, 0, NoSpan)

  class Info(details: Int):
    var lineCount: Int = 0
    var tokenCount: Int = 0
    var tastySize: Int = 0
    def complexity: Float = (tastySize/TastyChunkSize).toFloat/lineCount
    val leading: Array[MethodInfo] = Array.fill[MethodInfo](details)(NoInfo)

    def recordMethodSize(meth: Symbol, size: Int, span: Span): Unit =
      var i = leading.length
      while i > 0 && leading(i - 1).size < size do
        if i < leading.length then leading(i) = leading(i - 1)
        i -= 1
      if i < leading.length then
        leading(i) = MethodInfo(meth, size, span)
  end Info
end Profile

class ActiveProfile(details: Int) extends Profile:

  private val pinfo = new EqHashMap[CompilationUnit, Profile.Info]

  private val junkInfo = new Profile.Info(0)

  private def curInfo(using Context): Profile.Info =
    val unit: CompilationUnit | Null = ctx.compilationUnit
    if unit == null then junkInfo else unitProfile(unit)

  def unitProfile(unit: CompilationUnit): Profile.Info =
    pinfo.getOrElseUpdate(unit, new Profile.Info(details))

  def recordNewLine()(using Context): Unit =
    curInfo.lineCount += 1
  def recordNewToken()(using Context): Unit =
    curInfo.tokenCount += 1
  def recordTasty(size: Int)(using Context): Unit =
    curInfo.tastySize += size
  def recordMethodSize(meth: Symbol, size: Int, span: Span)(using Context): Unit =
    curInfo.recordMethodSize(meth, size, span)

  def printSummary()(using Context): Unit =
    val units =
      val rawUnits = pinfo.keysIterator.toArray
      ctx.settings.VprofileSortedBy.value match
        case "name"       => rawUnits.sortBy(_.source.file.name)
        case "path"       => rawUnits.sortBy(_.source.file.path)
        case "lines"      => rawUnits.sortBy(unitProfile(_).lineCount)
        case "tokens"     => rawUnits.sortBy(unitProfile(_).tokenCount)
        case "complexity" => rawUnits.sortBy(unitProfile(_).complexity)
        case _            => rawUnits.sortBy(unitProfile(_).tastySize)

    def printHeader(sourceNameWidth: Int, methNameWidth: Int = 0): String =
      val prefix =
        if methNameWidth > 0
        then s"%-${sourceNameWidth}s %-${methNameWidth}s".format("Sourcefile", "Method")
        else s"%-${sourceNameWidth}s".format("Sourcefile")
      val layout = s"%-${prefix.length}s %6s %8s %7s %s    %s"
      report.echo(layout.format(prefix, "Lines", "Tokens", "Tasty", " Complexity/Line", "Directory"))
      layout

    def printInfo(layout: String, name: String, info: Profile.Info, path: String) =
      val complexity = info.complexity
      val explanation =
        if complexity < 1       then "low     "
        else if complexity < 5  then "moderate"
        else if complexity < 25 then "high    "
        else                         "extreme "
      report.echo(layout.format(
        name, info.lineCount, info.tokenCount, info.tastySize/Profile.TastyChunkSize,
        s"${"%6.2f".format(complexity)}  $explanation", path))

    def safeMax(xs: Array[Int]) = xs.max.max(10).min(50)

    def printAndAggregateSourceInfos(): Profile.Info =
      val sourceNameWidth = safeMax(units.map(_.source.file.name.length))
      val layout = printHeader(sourceNameWidth)
      val agg = new Profile.Info(details)
      for unit <- units do
        val file = unit.source.file
        val info = unitProfile(unit)
        printInfo(layout, file.name, info, file.container.path)
        agg.lineCount += info.lineCount
        agg.tokenCount += info.tokenCount
        agg.tastySize += info.tastySize
        for Profile.MethodInfo(meth, size, span) <- info.leading do
          agg.recordMethodSize(meth, size, span)
      if units.length > 1 then
        report.echo(s"${"-" * sourceNameWidth}------------------------------------------")
        printInfo(layout, "Total", agg, "")
      agg

    def printDetails(agg: Profile.Info): Unit =
      val sourceNameWidth = safeMax(agg.leading.map(_.meth.source.name.length))
      val methNameWidth = safeMax(agg.leading.map(_.meth.name.toString.length))
      report.echo("\nMost complex methods:")
      val layout = printHeader(sourceNameWidth, methNameWidth)
      for
        Profile.MethodInfo(meth, size, span) <- agg.leading.reverse
        unit <- units.find(_.source.eq(meth.source))
      do
        val methProfile = new ActiveProfile(0)
        val methCtx = ctx.fresh.setCompilationUnit(unit)
        val s = Scanner(meth.source, span.start, methProfile)(using methCtx)
        while s.offset < span.end do s.nextToken()
        val info = methProfile.unitProfile(unit)
        info.tastySize = size
        val file = meth.source.file
        val header = s"%-${sourceNameWidth}s %-${methNameWidth}s".format(file.name, meth.name)
        printInfo(layout, header, info, file.container.path)

    val agg = printAndAggregateSourceInfos()
    if details > 0 then printDetails(agg)
  end printSummary
end ActiveProfile

object NoProfile extends Profile:
  def unitProfile(unit: CompilationUnit) = unsupported("NoProfile.info")
  def recordNewLine()(using Context): Unit = ()
  def recordNewToken()(using Context): Unit = ()
  def recordTasty(size: Int)(using Context): Unit = ()
  def recordMethodSize(meth: Symbol, size: Int, span: Span)(using Context): Unit = ()
  def printSummary()(using Context): Unit = ()
