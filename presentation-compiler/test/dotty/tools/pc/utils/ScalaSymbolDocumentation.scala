package dotty.tools.pc.utils

import scala.meta.pc.SymbolDocumentation
import scala.jdk.CollectionConverters._

case class ScalaSymbolDocumentation(
    symbol: String,
    displayName: String,
    docstring: String,
    defaultValue: String = "",
    typeParameters: java.util.List[SymbolDocumentation] = Nil.asJava,
    parameters: java.util.List[SymbolDocumentation] = Nil.asJava
) extends SymbolDocumentation
