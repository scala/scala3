package dotty.tools
package dotc
package typer

import core._
import Phases._
import Contexts._
import Symbols._
import ImportInfo.withRootImports
import parsing.{Parser => ParserPhase}
import config.Printers.typr
import inlines.PrepareInlineable
import util.Stats._

/**
 *
 * @param addRootImports Set to false in the REPL. Calling [[ImportInfo.withRootImports]] on the [[Context]]
 *                       for each [[CompilationUnit]] causes dotty.tools.repl.ScriptedTests to fail.
 */
class TyperPhase(addRootImports: Boolean = true) extends Phase {

  override def phaseName: String = TyperPhase.name

  override def description: String = TyperPhase.description

  override def isTyper: Boolean = true


  override def allowsImplicitSearch: Boolean = true

  // Run regardless of parsing errors
  override def isRunnable(implicit ctx: Context): Boolean = true

  def enterSyms(using Context): Unit = monitor("indexing") {
    val unit = ctx.compilationUnit
    ctx.typer.index(unit.untpdTree)
    typr.println("entered: " + unit.source)
  }

  def typeCheck(using Context): Unit = monitor("typechecking") {
    val unit = ctx.compilationUnit
    try
      if !unit.suspended then
        unit.tpdTree = ctx.typer.typedExpr(unit.untpdTree)
        typr.println("typed: " + unit.source)
        record("retained untyped trees", unit.untpdTree.treeSize)
        record("retained typed trees after typer", unit.tpdTree.treeSize)
        ctx.run.nn.suppressions.reportSuspendedMessages(unit.source)
    catch
      case ex: CompilationUnit.SuspendException =>
      case ex: Throwable =>
        println(s"$ex while typechecking $unit")
        throw ex
  }

  def javaCheck(using Context): Unit = monitor("checking java") {
    val unit = ctx.compilationUnit
    if unit.isJava then
      JavaChecks.check(unit.tpdTree)
  }

  /** Report unused imports.
   *
   *  If `-Yrewrite-imports`, emit patches instead.
   *  Patches are applied if `-rewrite` and no errors.
   */
  def emitDiagnostics(using Context): Unit =
    import ast.Trees.*
    import ast.untpd
    import parsing.Parsers
    import rewrites.Rewrites.patch
    import util.SourceFile
    import Decorators.*
    def reportSelectors(sels: List[untpd.ImportSelector]) = sels.foreach(sel => report.warning(s"Unused import", pos = sel.srcPos))
    // format the import clause, qual.{ selectors }
    def toText(qual: untpd.Tree, sels: List[untpd.ImportSelector]): String =
      def selected(sel: untpd.ImportSelector) =
        if sel.isGiven then
          if sel.bound.isEmpty then "given" else "given " + sel.bound.show
        else if sel.isWildcard then "*"
        else if sel.name == sel.rename then sel.name.show
        else s"${sel.name.show} as ${sel.rename.show}"
      val selections = sels.map(selected)
      if sels.length > 1 then selections.mkString(i"$qual.{", ", ", "}") else i"$qual.${selections.head}"
    end toText
    // begin
    val unused = ctx.usages.unused    //ImportInfo, owner Symbol, unused List[untpd.ImportSelector]
    if !ctx.settings.YrewriteImports.value then
      unused.foreach { (info, owner, selectors) => reportSelectors(selectors) }
    else if unused.nonEmpty then
      val byLocation = unused.groupBy((info, owner, selectors) => info.qualifier.sourcePos.withSpan(info.enclosingSpan))
      byLocation.foreach { (enclosingPos, grouped) =>
        val importText = enclosingPos.spanText
        val lineSource = SourceFile.virtual(name = "import-line.scala", content = importText)
        val PackageDef(_, clauses) = Parsers.Parser(lineSource).parse(): @unchecked
        val edited =
          clauses.map {
            case importTree @ Import(pqual, pselectors) =>
              //val importPrefix = raw"import\s+".r
              //val prologue = importPrefix.findPrefixMatchOf(importText).map(_.end).getOrElse(0)
              // uncorrupt span of first clause, which starts at keyword but really starts at point
              val span =
                if importTree.span.point != importTree.span.start then importTree.span.withStart(importTree.span.point)
                else importTree.span
              val zone = span.shift(enclosingPos.span.start)
              grouped.find { (info, _, _) => zone.contains(info.qualifier.span) } match
                case Some((_, _, selectors)) =>
                  val retained = pselectors.filterNot(psel => selectors.exists(_.sameTree(psel)))
                  (zone, pqual, retained, true)
                case _ =>
                  (zone, pqual, pselectors, false)
          }
        val src = ctx.compilationUnit.source
        // Simple deletion, or replace import statement with nonempty clauses; or if multiline, patch the emended bits
        if edited.forall(_._3.isEmpty) then
          val spanToDelete =
            val lines = enclosingPos.lineContent.linesIterator
            val line = lines.next()
            val (gutter, text) = line.splitAt(enclosingPos.startColumn)
            if !lines.hasNext && gutter.forall(Character.isWhitespace) && text == importText then
              enclosingPos.source.lineSpan(enclosingPos.point)
            else enclosingPos.span
          patch(src, spanToDelete, "")
        else if !importText.contains('\n') then
          val patched = edited.flatMap {
            case (_, qual, retained, _) if retained.nonEmpty => Some(toText(qual, retained))
            case _ => None
          }.mkString("import ", ", ", "")
          patch(src, enclosingPos.span, patched)
        else
          edited.zipWithIndex.foreach {
            case ((span, qual, retained, emended), index) =>
              if emended then
                if retained.isEmpty then
                  // adjust span to include a comma
                  val adjustedSpan =
                    if index == edited.length - 1 then span.withStart(edited(index - 1)._1.end) // TODO don't overlap
                    else span.withEnd(edited(1)._1.start)
                  patch(src, adjustedSpan, "")
                else
                  patch(src, span, toText(qual, retained))
          }
        end if
      }
  end emitDiagnostics

  def clearDiagnostics()(using Context): Unit =
    ctx.usages.clear()

  protected def discardAfterTyper(unit: CompilationUnit)(using Context): Boolean =
    unit.isJava || unit.suspended

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val unitContexts =
      for unit <- units yield
        val newCtx0 = ctx.fresh.setPhase(this.start).setCompilationUnit(unit)
        val newCtx = PrepareInlineable.initContext(newCtx0)
        report.inform(s"typing ${unit.source}")
        if (addRootImports)
          newCtx.withRootImports
        else
          newCtx

    unitContexts.foreach(enterSyms(using _))

    ctx.base.parserPhase match {
      case p: ParserPhase =>
        if p.firstXmlPos.exists && !defn.ScalaXmlPackageClass.exists then
          report.error(
            """To support XML literals, your project must depend on scala-xml.
              |See https://github.com/scala/scala-xml for more information.""".stripMargin,
            p.firstXmlPos)
      case _ =>
    }

    unitContexts.foreach(typeCheck(using _))
    record("total trees after typer", ast.Trees.ntrees)
    unitContexts.foreach(javaCheck(using _)) // after typechecking to avoid cycles

    unitContexts.foreach(emitDiagnostics(using _))
    clearDiagnostics()

    val newUnits = unitContexts.map(_.compilationUnit).filterNot(discardAfterTyper)
    ctx.run.nn.checkSuspendedUnits(newUnits)
    newUnits

  def run(using Context): Unit = unsupported("run")
}

object TyperPhase {
  val name: String = "typer"
  val description: String = "type the trees"
}

@deprecated(message = "FrontEnd has been split into TyperPhase and Parser. Refer to one or the other.")
object FrontEnd {
  // For backwards compatibility: some plugins refer to FrontEnd so that they can schedule themselves after it.
  val name: String = TyperPhase.name
}
