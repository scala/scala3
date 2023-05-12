package dotty.tools.pc.utils

import dotty.tools.dotc.core.Comments.CommentsContext
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.JavaDefined
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.ParsedComment
import dotty.tools.io.AbstractFile

import java.io.File
import scala.meta.internal.pc.SemanticdbSymbols
import scala.meta.pc.SymbolDocumentation

class TestingIndex(classpath: Seq[String]):

  val jdkIndex = JdkIndex()
  val defaultFlags = List("-color:never")

  val settings = defaultFlags ++
    List("-classpath", classpath.mkString(File.pathSeparator))

  val driver = new InteractiveDriver(settings)
  given Context = driver.currentCtx

  def search(query: String): List[AbstractFile] =
    val symbols = SemanticdbSymbols.inverseSemanticdbSymbol(query)
    symbols
      .distinctBy(symbol => if symbol.is(JavaDefined) then symbol.name.toString else symbol)
      .flatMap(symbol =>
        if !symbol.is(JavaDefined) then Option(symbol.sourcePos).map(_.source.file)
        else Option(symbol.associatedFile)
      )

  def getDoc(symbol: Symbol)(using ctx: Context) =
    for
      docCtx <- ctx.docCtx
      comment <- docCtx.docstring(symbol)
    yield comment

  def documentation(query: String): Option[SymbolDocumentation] =
    val symbols = SemanticdbSymbols.inverseSemanticdbSymbol(query)

    // we have to choose only one symbol because we receive both object and class symbols for java defined symbols
    val deduplicatedJavaSymbols = symbols
      .filter(_.is(JavaDefined))
      .distinctBy(sym => sym.name.toString)

    val adjustedSymbols = symbols.filterNot(_.is(JavaDefined)) ++ deduplicatedJavaSymbols

    adjustedSymbols
      .map(symbol =>
        val path = Option(symbol.associatedFile).map(_.path.toString)
        if symbol.is(JavaDefined) then path.flatMap(path => jdkIndex.search(path, symbol, query))
        else
          symbol.ensureCompleted()
          val docComment = ParsedComment.docOf(symbol)

          docComment.map { docString =>
            ScalaSymbolDocumentation(
              query,
              symbol.name.toString,
              docComment.map(_.renderAsMarkdown).getOrElse("")
            )
          }
      )
      .headOption
      .flatten
