package scala.tasty

import scala.tasty.reflect._

abstract class Tasty
    extends TastyCore
    with CaseDefOps
    with ConstantOps
    with ContextOps
    with IdOps
    with ImportSelectorOps
    with QuotedOps
    with PatternOps
    with PositionOps
    with Printers
    with SignatureOps
    with StandardDefinitions
    with SymbolOps
    with TreeOps
    with TypeOrBoundsTreeOps
    with TypeOrBoundsOps
