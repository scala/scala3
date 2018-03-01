package dotty.tools.dotc.printing

import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types.{ExprType, TypeAlias}
import dotty.tools.dotc.printing.Texts._

import scala.language.implicitConversions

class ReplPrinter(_ctx: Context) extends DecompilerPrinter(_ctx) {

  override def nameString(name: Name): String =
    if (name.isReplAssignName) name.decode.toString.takeWhile(_ != '$')
    else super.nameString(name)

  override protected def exprToText(tp: ExprType): Text =
    ": " ~ toText(tp.resType)

  override def toText(sym: Symbol): Text =
    if (sym.name.isReplAssignName) nameString(sym.name).toText
    else keyString(sym).toText ~~ nameString(sym.name.stripModuleClassSuffix)

  override def toText(const: Constant): Text =
    if (const.tag == Constants.StringTag) Str('"' + const.value.toString + '"')
    else Str(const.value.toString)

  override def dclText(sym: Symbol): Text = {
    toText(sym) ~ {
      if (sym.is(Method)) toText(sym.info)
      else if (sym.isType && sym.info.isInstanceOf[TypeAlias]) toText(sym.info)
      else if (sym.isType || sym.isClass) "".toText
      else ":".toText ~~ toText(sym.info)
    }
  }
}
