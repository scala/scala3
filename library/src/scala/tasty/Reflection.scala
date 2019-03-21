package scala.tasty

import scala.tasty.reflect._

abstract class Reflection
    extends Core
    with ConstantOps
    with ContextOps
    with CommentOps
    with FlagsOps
    with IdOps
    with ImportSelectorOps
    with QuotedOps
    with PatternOps
    with PositionOps
    with Printers
    with ReportingOps
    with RootPosition
    with SettingsOps
    with SignatureOps
    with StandardDefinitions
    with SymbolOps
    with TreeOps
    with TreeUtils
    with TypeOrBoundsOps { self =>

  def typeOf[T: scala.quoted.Type]: Type =
    implicitly[scala.quoted.Type[T]].unseal.tpe


  /** Whether the code type checks in the given context?
   *
   *  @param code The code to be type checked
   *
   *  The code should be a sequence of expressions or statements that may appear in a block.
   */
  def typeChecks(code: String)(implicit ctx: Context): Boolean = kernel.typeChecks(code)(ctx)

  val util: reflect.utils.TreeUtils { val reflect: self.type } = new reflect.utils.TreeUtils {
    val reflect: self.type = self
  }
}

object Reflection {
  /** Compiler tasty context available in a top level ~ of an inline macro */
  def macroContext: Reflection = throw new Exception("Not in inline macro.")
}
