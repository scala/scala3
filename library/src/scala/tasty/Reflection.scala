package scala.tasty

import scala.quoted.QuoteContext
import scala.tasty.reflect._

class Reflection(private[scala] val internal: CompilerInterface)
    extends Core
    with ConstantOps
    with ContextOps
    with CommentOps
    with FlagsOps
    with IdOps
    with ImplicitsOps
    with ImportSelectorOps
    with QuotedOps
    with PositionOps
    with PrinterOps
    with ReportingOps
    with RootPosition
    with SignatureOps
    with StandardDefinitions
    with SymbolOps
    with TreeOps
    with TreeUtils
    with TypeOrBoundsOps { self =>

  def typeOf[T: scala.quoted.Type]: Type =
    implicitly[scala.quoted.Type[T]].unseal.tpe

}
