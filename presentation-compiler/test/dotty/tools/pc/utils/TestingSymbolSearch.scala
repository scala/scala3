package dotty.tools.pc.utils

import dotty.tools.dotc.core.Contexts.Context
import org.eclipse.lsp4j.{Location, Position, Range}

import java.net.URI
import java.util as ju
import java.util.Optional
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.meta.internal.metals.{ClasspathSearch, WorkspaceSymbolQuery}
import scala.meta.pc.SymbolSearch.Result
import scala.meta.pc.{ParentSymbols, SymbolDocumentation, SymbolSearch, SymbolSearchVisitor}

/*
 * This symbol index is only used for testing purposes and should not be used
 * for other use cases. It is recommended to use own implementation.
 */
class TestingSymbolSearch(
    workspace: TestingWorkspaceSearch,
    maybeIndex: Option[TestingIndex],
    classpath: ClasspathSearch = ClasspathSearch.empty
) extends SymbolSearch:
  override def search(
      textQuery: String,
      buildTargetIdentifier: String,
      visitor: SymbolSearchVisitor
  ): SymbolSearch.Result =
    val query = WorkspaceSymbolQuery.exact(textQuery)
    workspace.search(query, visitor)
    classpath.search(query, visitor)

  override def searchMethods(
      textQuery: String,
      buildTargetIdentifier: String,
      visitor: SymbolSearchVisitor
  ): SymbolSearch.Result =
    val query = WorkspaceSymbolQuery.exact(textQuery)
    workspace.search(query, visitor, _.denot.isRealMethod)
    SymbolSearch.Result.COMPLETE

  override def definition(symbol: String, source: URI): ju.List[Location] =
    maybeIndex.toList
      .flatMap: index =>
        index
          .search(symbol)
          .map(source =>
            val name = source.name.replace("class", "java")
            val uri = s"$symbol $name"
            new Location(
              uri,
              new Range(new Position(0, 0), new Position(0, 0))
            )
          )
      .asJava

  override def definitionSourceToplevels(
      symbol: String,
      sourceUri: URI
  ): ju.List[String] =
    maybeIndex.toList
      .flatMap: index =>
        index
          .search(symbol)
          .filter(_.`extension` == "class")
          .map(_.name.stripSuffix(".class"))
      .asJava

  override def documentation(
      symbol: String,
      parents: ParentSymbols
  ): Optional[SymbolDocumentation] =
    maybeIndex
      .flatMap: index =>
        (symbol +: parents.parents().asScala).iterator
          .map(index.documentation)
          .collectFirst { case Some(doc) =>
            doc
          }
      .toJava
