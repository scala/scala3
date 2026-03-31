package dotty.tools.pc.utils

import java.net.URI
import java.util.Optional
import java.util as ju

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.language.unsafeNulls
import scala.meta.internal.metals.{ClasspathSearch, WorkspaceSymbolQuery}
import scala.meta.pc.{ParentSymbols, SymbolDocumentation, SymbolSearch, SymbolSearchVisitor}
import scala.meta.pc.ContentType

import org.eclipse.lsp4j.Location

/* Mocking symbol search is an external part of presentation compiler,
 * which is used for 3 purposes: finding Symbols that match our query,
 * their definition location or documentation based on semanticdb symbol.
 *
 * This mock has to be provided to test presentation compiler implementations,
 * which consumes returned symbols from symbol search. The main functinalities that rely on it
 * are completion enrichements, go to definition and type definition, hover and signature help.
 *
 * @param workspace is used to search for Symbols in the workspace
 * @param classpathSearch is used to search for Symbols in the classpath
 * @param mockEntries is used to find mocked definitions and documentations
 */
class MockSymbolSearch(
    workspace: TestingWorkspaceSearch,
    classpath: ClasspathSearch,
    mockEntries: MockEntries
) extends SymbolSearch:

  override def search(
      textQuery: String,
      buildTargetIdentifier: String,
      visitor: SymbolSearchVisitor
  ): SymbolSearch.Result =
    val query = WorkspaceSymbolQuery.exact(textQuery)
    workspace.search(query, visitor)
    classpath.search(query, visitor)._1

  override def searchMethods(
      textQuery: String,
      buildTargetIdentifier: String,
      visitor: SymbolSearchVisitor
  ): SymbolSearch.Result =
    val query = WorkspaceSymbolQuery.exact(textQuery)
    workspace.search(query, visitor)
    SymbolSearch.Result.COMPLETE

  override def definition(symbol: String, source: URI): ju.List[Location] =
    mockEntries.definitions.getOrElse(symbol, Nil).asJava

  override def definitionSourceToplevels(
      symbol: String,
      sourceUri: URI
  ): ju.List[String] =
    mockEntries.definitionSourceTopLevels.getOrElse(symbol, Nil).asJava

  override def documentation(
      symbol: String,
      parents: ParentSymbols
  ): Optional[SymbolDocumentation] = documentation(symbol, parents, ContentType.MARKDOWN)

  override def documentation(
      symbol: String,
      parents: ParentSymbols,
      contentType: ContentType
  ): Optional[SymbolDocumentation] =
    (symbol +: parents.parents().asScala).iterator
      .map(symbol => mockEntries.documentations.find(_.symbol == symbol))
      .collectFirst { case Some(doc) => doc }
      .toJava
