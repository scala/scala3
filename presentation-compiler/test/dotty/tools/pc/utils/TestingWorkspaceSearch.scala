package dotty.tools.pc.utils

import java.io.File
import java.nio.file.Paths

import scala.collection.mutable
import scala.meta.internal.metals.{
  CompilerVirtualFileParams,
  Fuzzy,
  WorkspaceSymbolQuery
}
import scala.meta.pc.SymbolSearchVisitor
import scala.language.unsafeNulls

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.semanticdb.SemanticSymbolBuilder
import dotty.tools.pc.CompilerSearchVisitor

object TestingWorkspaceSearch:
  def empty: TestingWorkspaceSearch = new TestingWorkspaceSearch(Nil)

class TestingWorkspaceSearch(classpath: Seq[String]):
  val inputs: mutable.Map[String, String] = mutable.Map.empty[String, String]

  val defaultFlags = List("-color:never")

  val settings =
    defaultFlags ++
      List("-classpath", classpath.mkString(File.pathSeparator))

  val driver = new InteractiveDriver(settings)

  def search(
      query: WorkspaceSymbolQuery,
      visitor: SymbolSearchVisitor,
      filter: Symbol => Context ?=> Boolean = _ => true
  ): Unit =
    given Context = driver.currentCtx.fresh

    visitor match
      case visitor: CompilerSearchVisitor =>
        inputs.map { (path, text) =>

          val nioPath = Paths.get(path)
          val uri = nioPath.toUri()
          val symbols =
            DefSymbolCollector(
              driver,
              CompilerVirtualFileParams(uri, text)
            ).namedDefSymbols

          // We have to map symbol from this Context, to one in PresentationCompiler
          // To do it we are searching it with semanticdb symbol
          val semanticSymbolBuilder = SemanticSymbolBuilder()
          symbols
            .filter((symbol, _) => filter(symbol))
            .filter((_, name) => Fuzzy.matches(query.query, name))
            .map(symbol => semanticSymbolBuilder.symbolName(symbol._1))
            .map(
              visitor.visitWorkspaceSymbol(Paths.get(""), _, null, null)
            )
        }
      case _ =>
