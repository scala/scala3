package dotty.tools.dotc.printing

import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.printing.Texts._


class ReplPrinter(_ctx: Context) extends DecompilerPrinter(_ctx) {

  override def nameString(name: Name): String =
    if (name.isReplAssignName) name.decode.toString.takeWhile(_ != '$')
    else super.nameString(name)

  override protected def exprToText(tp: ExprType): Text =
    ": " ~ toText(tp.resType)

  override def toText(sym: Symbol): Text =
    if (sym.name.isReplAssignName) nameString(sym.name)
    else keyString(sym) ~~ nameString(sym.name.stripModuleClassSuffix)

  override def toText(const: Constant): Text =
    if (const.tag == Constants.StringTag) Str('"' + const.value.toString + '"')
    else Str(const.value.toString)

  override def dclText(sym: Symbol): Text = {
    ("lazy": Text).provided(sym.is(Lazy)) ~~
    toText(sym) ~ {
      if (sym.is(Method)) toText(sym.info)
      else if (sym.isType && sym.info.isTypeAlias) toText(sym.info)
      else if (sym.isType || sym.isClass) ""
      else ":" ~~ toText(sym.info)
    }
  }

  override def toTextSingleton(tp: SingletonType): Text = tp match {
    case ConstantType(const) => toText(const)
    case _                   => toTextRef(tp) ~ ".type"
  }

  // We don't want the colors coming from RefinedPrinter as the REPL uses its
  // own syntax coloring mechanism.
  override def coloredStr(text: String, color: String): String = text
  override def coloredText(text: Text, color: String): Text = text
}
