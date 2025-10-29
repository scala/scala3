package dotty.tools.dotc.printing

import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.printing.Texts.*


class ReplPrinter(_ctx: Context) extends RefinedPrinter(_ctx) {

  val debugPrint = _ctx.settings.YprintDebug.value

  override def nameString(name: Name): String =
    if (name.isReplAssignName) name.decode.toString.takeWhile(_ != '$')
    else super.nameString(name)

  override def toText(sym: Symbol): Text =
    if (sym.name.isReplAssignName) nameString(sym.name)
    else if (debugPrint) super.toText(sym)
    else keyString(sym) ~~ nameString(sym.name.stripModuleClassSuffix)

  inline private val qSc = '"';

  override def toText(const: Constant): Text =
    if (debugPrint) super.toText(const)
    else if (const.tag == Constants.StringTag) Str(s"${qSc}${const.value}$qSc")
    else Str(const.value.toString)

  override def dclText(sym: Symbol): Text = if (debugPrint) super.dclText(sym) else
    ("lazy": Text).provided(sym.is(Lazy)) ~~
    toText(sym) ~ {
      if (sym.is(Method)) {
        sym.info match {
          case tp: ExprType => ":" ~~ toText(tp.resType)
          case info => toText(info)
        }
      }
      else if (sym.isType && sym.info.isTypeAlias) toText(sym.info)
      else if (sym.isType || sym.isClass) ""
      else ":" ~~ toText(sym.info)
    }

  override def toTextSingleton(tp: SingletonType): Text =
    if (debugPrint)
      super.toTextSingleton(tp)
    else
      tp match {
        case ConstantType(const) => toText(const)
        case _                   => toTextRef(tp) ~ ".type"
      }

  // We don't want the colors coming from RefinedPrinter as the REPL uses its
  // own syntax coloring mechanism.
  override def coloredStr(text: String, color: dotty.shaded.fansi.Attrs): String = text
  override def coloredText(text: Text, color: dotty.shaded.fansi.Attrs): Text = text
}
