package dotty.tools.pc.utils

import scala.jdk.CollectionConverters.*
import scala.meta.pc.SymbolDocumentation

case class ScalaSymbolDocumentation(
    symbol: String,
    displayName: String,
    docstring: String,
    defaultValue: String = "",
    typeParameters: java.util.List[SymbolDocumentation] = Nil.asJava,
    parameters: java.util.List[SymbolDocumentation] = Nil.asJava
) extends SymbolDocumentation
