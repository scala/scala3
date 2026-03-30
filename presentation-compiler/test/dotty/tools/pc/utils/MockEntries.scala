package dotty.tools.pc.utils

import scala.jdk.CollectionConverters.*
import scala.meta.pc.SymbolDocumentation

import org.eclipse.lsp4j.{Location, Position, Range}

/* This is a mock class which is used to test presentation compiler
 * implementations. It contains mocked definitions and documentations.
 *
 * The definitions are used to test go to definition and type definition, to
 * ensure that the presentation compiler returns the correct semanticdb symbol,
 * that will be used to query the symbol index.
 *
 * The documentation is used to test e.g. java symbol param name replacement or
 * to extract documentation for given semanticdb symbol.
 */
abstract class MockEntries:
  object MockLocation:
    def apply(symbol: String, path: String): (String, List[Location]) =
      symbol -> List(
        Location(s"$symbol $path", Range(Position(0, 0), Position(0, 0)))
      )

  case class MockParam(name: String, defaultValue: String = "")

  object ScalaMockDocumentation:
    def apply(
        symbol: String,
        displayName: String,
        typeParams: List[MockParam] = Nil,
        params: List[MockParam] = Nil
    ): SymbolDocumentation =
      ScalaSymbolDocumentation(
        symbol,
        displayName,
        s"Found documentation for $symbol",
        "",
        typeParams
          .map: param =>
            ScalaSymbolDocumentation(
              param.name,
              param.name,
              s"Found documentation for type param ${param.name}\n"
            )
          .asJava,
        params
          .map(param =>
            ScalaSymbolDocumentation(
              param.name,
              param.name,
              s"Found documentation for param ${param.name}\n",
              param.defaultValue
            )
          )
          .asJava
      )

  object MockDocumentation:
    def apply(
        symbol: String,
        displayName: String,
        typeParameterNames: Seq[String] = Seq(),
        paramNames: Seq[String] = Seq()
    ) =
      ScalaSymbolDocumentation(
        symbol,
        displayName,
        "",
        "",
        typeParameterNames
          .map(ScalaSymbolDocumentation("", _, "", "", Nil.asJava, Nil.asJava))
          .asJava,
        paramNames
          .map(ScalaSymbolDocumentation("", _, "", "", Nil.asJava, Nil.asJava))
          .asJava
      )

  def definitions: Map[String, List[Location]] = Map.empty
  def definitionSourceTopLevels: Map[String, List[String]] = Map.empty
  def documentations: Set[SymbolDocumentation] = Set.empty
