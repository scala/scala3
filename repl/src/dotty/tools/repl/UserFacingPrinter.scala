package dotty.tools
package repl

import dotc.ast.Trees.{ Untyped, Tree }
import dotc.core.Annotations.Annotation
import dotc.core.Constants.Constant
import dotc.core.Contexts.Context
import dotc.core.Denotations.{ Denotation, MultiDenotation, SingleDenotation }
import dotc.core.Flags._
import dotc.core.TypeApplications.AppliedType
import dotc.core.Names._
import dotc.core.Decorators._
import dotc.core.Scopes.Scope
import dotc.core.Symbols.{ Symbol, ClassSymbol, defn }
import dotc.core.SymDenotations.NoDenotation
import dotc.core.Types._
import dotc.printing.Texts._
import dotc.printing.{ Printer, PlainPrinter }
import dotc.printing.GlobalPrec
import dotc.typer.Implicits.SearchResult
import dotc.typer.ImportInfo

class UserFacingPrinter(_ctx: Context) extends Printer {

  protected[this] implicit def ctx: Context = _ctx

  private def panic(msg: String): Nothing = { assert(false, msg); ??? }

  private[this] def getPkgCls(path: String) =
    _ctx.requiredPackage(path).moduleClass.asClass

  private lazy val collectionPkg = getPkgCls("scala.collection")
  private lazy val immutablePkg  = getPkgCls("scala.collection.immutable")
  private lazy val scalaPkg      = defn.ScalaPackageClass
  private lazy val javaLangPkg   = defn.JavaLangPackageVal.moduleClass.asClass

  def wellKnownPkg(pkgSym: Symbol) = pkgSym match {
    case `scalaPkg` | `collectionPkg` | `immutablePkg` | `javaLangPkg` => true
    case pkgSym if pkgSym.name.decode.show.contains("ReplSession$") => true
    case _ => false
  }

  def kindString(sym: Symbol): String = {
    val flags = sym.flags
    if (flags is Package) ""
    else if (sym.isPackageObject) "package object"
    else if (flags is Module) "object"
    else if (flags is ImplClass) "class"
    else if (flags.is(Trait)) "trait"
    else if (sym.isClass) "class"
    else if (sym.isType) "type"
    else if (flags.is(Lazy)) "lazy val"
    else if (flags.is(Mutable)) "var"
    else if (sym.is(Method)) "def"
    else if (sym.isTerm) "val"
    else ???
  }

  def nameString(name: Name): String = name match {
    case name: SimpleName => name.decode.toString
    case name: TypeName => name.decode.toString
  }

  def nameString(sym: Symbol): String = ???

  def fullNameString(sym: Symbol): String = ???

  def toText(name: Name): Text = nameString(name)

  def toText(sym: Symbol): Text =
    kindString(sym) ~~ nameString(sym.name)

  def dclText(sym: Symbol): Text =
    toText(sym) ~ {
      if (sym.is(Method)) toText(sym.info)
      else if (sym.isClass) ""
      else ":" ~~ toText(sym.info)
    }

  def dclText(sd: SingleDenotation): Text = ???

  def locationText(sym: Symbol): Text = ???

  def locatedText(sym: Symbol): Text = ???

  def extendedLocationText(sym: Symbol): Text = ???

  def toText(denot: Denotation): Text = denot match {
    case NoDenotation =>
      panic("NoDenotation encountered in UserFacingPrinter")
    case denot: MultiDenotation =>
      panic("MultiDenotation not allowed in UserFacingPrinter")
    case _ =>
      toText(denot.symbol)
  }

  def toText(const: Constant): Text = Str(const.value.toString)

  def toText(annot: Annotation): Text = ???

  def toText(tp: Type): Text = tp match {
    case tp: ConstantType => toText(tp.value)
    case tp: ParamRef => {
      if (tp.isInstanceOf[TermParamRef]) toText(tp.paramName) ~ ".type"
      else toText(tp.paramName)
    }
    case PolyType(params, res) =>
      "[" ~ Fluid(params.map(tl => toText(tl.toArg)).intersperse(Str(", "))) ~ "]" ~ toText(res)
    case tp: MethodType => {
      def paramText(name: TermName, tp: Type) = toText(name) ~ ": " ~ toText(tp)
      changePrec(GlobalPrec) {
        (if (tp.isImplicit) "(implicit " else "(") ~
          Text((tp.paramNames, tp.paramInfos).zipped map paramText, ", ") ~
        (if (tp.resultType.isInstanceOf[MethodType]) ")" else "): ") ~
        toText(tp.resultType)
      }
    }
    case ExprType(result) => ":" ~~ toText(result)
    case AppliedType(tp, xs) =>
      toText(tp) ~ "[" ~ Fluid(xs.reverse.map(toText).intersperse(Str(", "))) ~ "]"
    case tp: TypeRef => tp.info match {
      case TypeAlias(alias) => toText(alias)
      case _ => toText(tp.info)
    }
    case tp: ClassInfo => if (wellKnownPkg(tp.cls.owner)) nameString(tp.cls.name) else {
        def printPkg(sym: ClassSymbol): Text =
          if (sym.owner == defn.RootClass) toText(sym)
          else printPkg(sym.owner.asClass) ~ "." ~ toText(sym)

        printPkg(tp.cls.owner.asClass) ~ "." ~ nameString(tp.cls.name)
      }
  }

  def dclsText(syms: List[Symbol], sep: String): Text =
    Text(syms.map(dclText), sep)

  def toText(sc: Scope): Text = ???

  def toText[T >: Untyped](tree: Tree[T]): Text = ???

  def toText(result: SearchResult): Text = ???

  def toText(result: ImportInfo): Text = ???

  def summarized[T](depth: Int)(op: => T): T = ???

  lazy val plain = new PlainPrinter(_ctx)
}
