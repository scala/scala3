package scala.tasty

import scala.tasty.reflect._

class Reflection(val kernel: Kernel)
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
    with SignatureOps
    with StandardDefinitions
    with SymbolOps
    with TreeOps
    with TreeUtils
    with TypeOrBoundsOps { self =>

  def typeOf[T: scala.quoted.Type]: Type =
    implicitly[scala.quoted.Type[T]].unseal.tpe

  object typing {
   /** Whether the code type checks in the given context?
    *
    *  @param code The code to be type checked
    *
    *  @return false if the code has syntax error or type error in the given context, otherwise returns true.
    *
    *  The code should be a sequence of expressions or statements that may appear in a block.
    */
    def typeChecks(code: String)(implicit ctx: Context): Boolean = kernel.typeChecks(code)(ctx)
  }

  val util: reflect.utils.TreeUtils { val reflect: self.type } = new reflect.utils.TreeUtils {
    val reflect: self.type = self
  }
}

object Reflection {
  /** Compiler tasty context available in a top level ~ of an inline macro */
  def macroContext: Reflection = throw new Exception("Not in inline macro.")
}
