package dotty.tools.dotc
package printing

import core._
import Constants.Constant, Contexts.Context, Denotations._, Flags._, Names._
import NameOps._, StdNames._, Decorators._, Scopes.Scope, Types._, Texts._
import SymDenotations.NoDenotation, Symbols.{ Symbol, ClassSymbol, defn }

class UserFacingPrinter(_ctx: Context) extends RefinedPrinter(_ctx) {

  private[this] def getPkgCls(path: String) =
    _ctx.requiredPackage(path).moduleClass.asClass

  private lazy val collectionPkg = getPkgCls("scala.collection")
  private lazy val immutablePkg  = getPkgCls("scala.collection.immutable")
  private lazy val scalaPkg      = defn.ScalaPackageClass
  private lazy val javaLangPkg   = defn.JavaLangPackageVal.moduleClass.asClass

  def standardPkg(pkgSym: Symbol) = pkgSym match {
    case `scalaPkg` | `collectionPkg` | `immutablePkg` | `javaLangPkg` => true
    case _ => false
  }

  def wrappedName(pkgSym: Symbol) =
    pkgSym.name.toTermName == nme.EMPTY_PACKAGE ||
    pkgSym.name.isReplWrapperName

  def wellKnownPkg(pkgSym: Symbol) = standardPkg(pkgSym) || wrappedName(pkgSym)

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

  override def argText(tp: Type): Text = tp match {
    case arg: TypeArgRef => argText(arg.underlying)
    case _ => super.argText(tp)
  }

  override def toText(tp: Type): Text = tp match {
    case ExprType(result) => ":" ~~ toText(result)
    case tp: ConstantType => toText(tp.value)
    case tp: TypeRef => tp.info match {
      case TypeAlias(alias) => toText(alias)
      case _ => toText(tp.info)
    }
    case tp: ClassInfo => {
      if (wellKnownPkg(tp.cls.owner)) nameString(tp.cls.name)
      else {
        def printPkg(sym: ClassSymbol): Text =
          if (sym.owner == defn.RootClass || wrappedName(sym.owner))
            nameString(sym.name.stripModuleClassSuffix)
          else
            printPkg(sym.owner.asClass) ~ "." ~ toText(sym)

        printPkg(tp.cls.owner.asClass) ~ "." ~ nameString(tp.cls.name)
      }
    }
    case tp => super.toText(tp)
  }
}
