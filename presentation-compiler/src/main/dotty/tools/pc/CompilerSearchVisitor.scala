package dotty.tools.pc

import java.util.logging.Level
import java.util.logging.Logger

import scala.meta.internal.metals.Report
import scala.meta.internal.metals.ReportContext
import scala.meta.pc.*
import scala.util.control.NonFatal

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Symbols.*

class CompilerSearchVisitor(
    visitSymbol: Symbol => Boolean
)(using ctx: Context, reports: ReportContext)
    extends SymbolSearchVisitor:

  val logger: Logger = Logger.getLogger(classOf[CompilerSearchVisitor].getName().nn).nn

  private def isAccessible(sym: Symbol): Boolean = try
    sym != NoSymbol && sym.isPublic && sym.isStatic
  catch
    case err: AssertionError =>
      logger.log(Level.WARNING, err.getMessage())
      false
    case NonFatal(e) =>
      reports.incognito.create(
        Report(
          "is_public",
          s"""Symbol: $sym""".stripMargin,
          e
        )
      )
      logger.log(Level.SEVERE, e.getMessage(), e)
      false

  private def toSymbols(
      pkg: String,
      parts: List[String]
  ): List[Symbol] =
    def loop(owners: List[Symbol], parts: List[String]): List[Symbol] =
      parts match
        case head :: tl =>
          val next = owners.flatMap { sym =>
            val term = sym.info.member(termName(head))
            val tpe = sym.info.member(typeName(head))

            List(term, tpe)
              .filter(denot => denot.exists)
              .map(_.symbol)
              .filter(isAccessible)
          }
          loop(next, tl)
        case Nil => owners

    val pkgSym = requiredPackage(pkg)
    loop(List(pkgSym), parts)
  end toSymbols

  def visitClassfile(pkgPath: String, filename: String): Int =
    val pkg = normalizePackage(pkgPath)

    val innerPath = filename
      .stripSuffix(".class")
      .stripSuffix("$")
      .split("\\$")

    val added =
      try toSymbols(pkg, innerPath.nn.toList.map(_.nn)).filter(visitSymbol)
      catch
        case NonFatal(e) =>
          logger.log(Level.WARNING, e.getMessage(), e)
          Nil
    added.size
  end visitClassfile

  def visitWorkspaceSymbol(
      path: java.nio.file.Path,
      symbol: String,
      kind: org.eclipse.lsp4j.SymbolKind,
      range: org.eclipse.lsp4j.Range
  ): Int =
    val gsym = SemanticdbSymbols.inverseSemanticdbSymbol(symbol).headOption
    gsym
      .filter(isAccessible)
      .map(visitSymbol)
      .map(_ => 1)
      .getOrElse(0)

  def shouldVisitPackage(pkg: String): Boolean =
    isAccessible(requiredPackage(normalizePackage(pkg)))

  override def isCancelled: Boolean = false

  private def normalizePackage(pkg: String): String =
    pkg.replace("/", ".").nn.stripSuffix(".")

end CompilerSearchVisitor
