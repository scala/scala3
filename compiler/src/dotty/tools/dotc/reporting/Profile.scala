package dotty.tools
package dotc
package reporting

import core.*
import Contexts.{Context, ctx}
import collection.mutable
import util.{EqHashMap, NoSourcePosition}

abstract class Profile:
  def unitProfile(unit: CompilationUnit): Profile.Info
  def recordNewLine()(using Context): Unit
  def recordNewToken()(using Context): Unit
  def recordTasty(size: Int)(using Context): Unit
  def printSummary()(using Context): Unit

object Profile:
  def current(using Context): Profile =
    val run = ctx.run
    if run == null then NoProfile else run.profile

  private val TastyFactor = 50

  class Info:
    var lineCount: Int = 0
    var tokenCount: Int = 0
    var tastySize: Int = 0
    def complexity: Float = tastySize.toFloat/lineCount/TastyFactor
end Profile

class ActiveProfile extends Profile:

  private val pinfo = new EqHashMap[CompilationUnit, Profile.Info]

  private val junkInfo = new Profile.Info

  private def curInfo(using Context): Profile.Info =
    val unit: CompilationUnit | Null = ctx.compilationUnit
    if unit == null then junkInfo else unitProfile(unit)

  def unitProfile(unit: CompilationUnit): Profile.Info =
    pinfo.getOrElseUpdate(unit, new Profile.Info)

  def recordNewLine()(using Context): Unit =
    curInfo.lineCount += 1
  def recordNewToken()(using Context): Unit =
    curInfo.tokenCount += 1
  def recordTasty(size: Int)(using Context): Unit =
    curInfo.tastySize += size

  def printSummary()(using Context): Unit =
    val units =
      val rawUnits = pinfo.keysIterator.toArray
      ctx.settings.YprofileSortedBy.value match
        case "name"       => rawUnits.sortBy(_.source.file.name)
        case "path"       => rawUnits.sortBy(_.source.file.path)
        case "lines"      => rawUnits.sortBy(unitProfile(_).lineCount)
        case "tokens"     => rawUnits.sortBy(unitProfile(_).tokenCount)
        case "complexity" => rawUnits.sortBy(unitProfile(_).complexity)
        case _            => rawUnits.sortBy(unitProfile(_).tastySize)

    val nameWidth = units.map(_.source.file.name.length).max.max(10).min(50)
    val layout = s"%-${nameWidth}s %6s %8s %8s %s     %s"
    report.echo(layout.format("Source file", "Lines", "Tokens", "Tasty", " Complexity/Line", "Directory"))

    def printInfo(name: String, info: Profile.Info, path: String) =
      val complexity = info.complexity
      val explanation =
        if complexity < 1       then "low     "
        else if complexity < 5  then "moderate"
        else if complexity < 25 then "high    "
        else                         "extreme "
      val complexityStr = s"${"%6.2f".format(info.complexity)}  $explanation"
      report.echo(layout.format(
        name, info.lineCount, info.tokenCount, info.tastySize, complexityStr, path))

    val agg = new Profile.Info
    for unit <- units do
      val info = unitProfile(unit)
      val file = unit.source.file
      printInfo(file.name, info, file.container.path)
      agg.lineCount += info.lineCount
      agg.tokenCount += info.tokenCount
      agg.tastySize += info.tastySize
    if units.length > 1 then
      report.echo(s"${"-"*nameWidth}------------------------------------------")
      printInfo("Total", agg, "")
  end printSummary
end ActiveProfile

object NoProfile extends Profile:
  def unitProfile(unit: CompilationUnit) = unsupported("NoProfile.info")
  def recordNewLine()(using Context): Unit = ()
  def recordNewToken()(using Context): Unit = ()
  def recordTasty(size: Int)(using Context): Unit = ()
  def printSummary()(using Context): Unit = ()
