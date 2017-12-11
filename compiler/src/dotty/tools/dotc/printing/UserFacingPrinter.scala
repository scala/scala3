package dotty.tools.dotc
package printing

import core._
import Constants.Constant, Contexts.Context, Denotations._, Flags._, Names._
import NameOps._, StdNames._, Decorators._, Scopes.Scope, Types._, Texts._
import SymDenotations.NoDenotation, Symbols.{ Symbol, ClassSymbol, defn }

class UserFacingPrinter(_ctx: Context) extends RefinedPrinter(_ctx) {

  private[this] def getPkgCls(path: String) =
    _ctx.requiredPackage(path).moduleClass.asClass

  override protected def keyString(sym: Symbol): String =
    if (sym.flagsUNSAFE is Package) "" else super.keyString(sym)

  override def nameString(name: Name): String =
    if (name.isReplAssignName) name.decode.toString.takeWhile(_ != '$')
    else name.decode.toString

  override def toText(sym: Symbol): Text =
    if (sym.name.isReplAssignName) nameString(sym.name)
    else keyString(sym) ~~ nameString(sym.name.stripModuleClassSuffix)

  override def dclText(sym: Symbol): Text = toText(sym) ~ {
    if (sym.is(Method)) toText(sym.info)
    else if (sym.isType && sym.info.isInstanceOf[TypeAlias]) toText(sym.info)
    else if (sym.isType || sym.isClass) ""
    else ":" ~~ toText(sym.info)
  }

  override def toText(const: Constant): Text = Str(const.value.toString)

  override def toText(tp: Type): Text = tp match {
    case ExprType(result) => ":" ~~ toText(result)
    case tp: ConstantType => toText(tp.value)
    case tp => super.toText(tp)
  }
}
