package scala.tasty

import scala.tasty.reflect._

abstract class Reflection
    extends ReflectionCore
    with CaseDefOps
    with ConstantOps
    with ContextOps
    with IdOps
    with ImportSelectorOps
    with QuotedOps
    with PatternOps
    with PositionOps
    with Printers
    with SettingsOps
    with SignatureOps
    with StandardDefinitions
    with SymbolOps
    with TreeOps
    with TypeOrBoundsTreeOps
    with TypeOrBoundsOps

object Reflection {
  /** Compiler tasty context available in a top level ~ of an inline macro */
  def macroContext: Reflection = throw new Exception("Not in inline macro.")
}
