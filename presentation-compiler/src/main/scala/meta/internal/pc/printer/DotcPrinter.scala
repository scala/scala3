package scala.meta.internal.pc.printer

import scala.meta.internal.pc.IndexedContext
import scala.meta.internal.pc.printer.ShortenedNames.PrettyType

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.printing.Texts.Text

/**
 * A limited subset of function that we use from compiler's printer
 */
trait DotcPrinter:
  def name(sym: Symbol): String
  def fullName(sym: Symbol): String
  def keywords(sym: Symbol): String
  def tpe(t: Type): String

object DotcPrinter:

  private val defaultWidth = 1000

  class Std(using ctx: Context)
      extends RefinedDotcPrinter(ctx)
      with DotcPrinter:

    override def nameString(name: Name): String =
      super.nameString(name.stripModuleClassSuffix)

    def name(sym: Symbol): String =
      nameString(sym)

    def keywords(sym: Symbol): String =
      keyString(sym)

    def tpe(t: Type): String =
      toText(t).mkString(defaultWidth, false)

    /* Overriden method because the RefinedPrinter will omit
     * `lang` even if it's needed in case of conflicts.
     *
     * Modified from:
     * https://github.com/lampepfl/dotty/blob/75d8eea1e943bbd2c605d8411c2d52d69974d6f3/compiler/src/dotty/tools/dotc/printing/RefinedPrinter.scala#L107-L123
     */
    override def toTextPrefix(tp: Type): Text = controlled {
      tp match
        case tp @ TermRef(pre, _) =>
          val sym = tp.symbol
          if sym.isPackageObject && !homogenizedView && !printDebug then
            toTextPrefix(pre)
          else printPrefix(tp)
        case _ => super.toTextPrefix(tp)
    }

    /* Taken from PlainPrinter's `toTextPrefix` in order not to use super
     * RefinedPrinter's `toTextPrefix` method.
     */
    private def printPrefix(tp: Type): Text = homogenize(tp) match
      case NoPrefix => ""
      case tp: SingletonType => toTextRef(tp) ~ "."
      case tp => trimPrefix(toTextLocal(tp)) ~ "#"

    def fullName(sym: Symbol): String =
      fullNameString(sym)

    override def toText(tp: Type): Text =
      // Override the behavior for `AppliedType` because
      // `toText` in RefinedPrinter doesn't pretty print AppliedType
      // if tycon is instance of PrettyType.
      // For example, if we don't override `toText`,
      // CompletionoverrideConfigSuite's `package` test will fail with
      // completing `def function: Int <none> String = ${0:???}`
      // instead of `def function: f.Function[Int, String] = ${0:???}`
      tp match
        case c: ConstantType =>
          toText(c.value)
        case tp: AppliedType =>
          tp.tycon match
            case p: PrettyType =>
              Str(p.toString) ~ "[" ~ toText(tp.args, ", ") ~ "]"
            case other => super.toText(tp)
        case other => super.toText(tp)
  end Std

  /**
   * This one is used only for adding inferred type
   * The difference with std is that in case of name clash it prepends `_root_`
   */
  class ForInferredType(indexed: IndexedContext) extends Std(using indexed.ctx):
    override def toTextPrefix(tp: Type): Text = controlled {
      tp match
        case tp: ThisType =>
          tp.tref match
            case tpe @ TypeRef(NoPrefix, designator) =>
              val sym =
                if designator.isInstanceOf[Symbol] then
                  designator.asInstanceOf[Symbol]
                else tpe.termSymbol

              val text = super.toTextPrefix(tp)
              if sym.is(ModuleClass) && indexed.toplevelClashes(sym) then
                Str("_root_.") ~ text
              else text
            case _ => super.toTextPrefix(tp)
        case _ => super.toTextPrefix(tp)
    }
  end ForInferredType
end DotcPrinter
