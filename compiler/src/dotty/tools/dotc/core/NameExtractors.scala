package dotty.tools
package dotc
package core

import Names._
import NameOps._
import StdNames._
import util.DotClass
import tasty.TastyFormat._
import Decorators._
import Contexts.Context
import collection.mutable

object NameKinds {

  @sharable private val simpleNameKinds = new mutable.HashMap[Int, ClassifiedNameKind]
  @sharable private val uniqueNameKinds = new mutable.HashMap[String, UniqueNameKind]
  @sharable private val qualifiedNameKinds = new mutable.HashMap[String, QualifiedNameKind]

  abstract class NameInfo extends DotClass {
    def kind: NameKind
    def mkString(underlying: TermName): String
    def map(f: SimpleTermName => SimpleTermName): NameInfo = this
  }

  abstract class NameKind(val tag: Int) extends DotClass { self =>
    type ThisInfo <: Info
    class Info extends NameInfo { this: ThisInfo =>
      def kind = self
      def mkString(underlying: TermName) = self.mkString(underlying, this)
      override def toString = infoString
    }
    def definesNewName = false
    def mkString(underlying: TermName, info: ThisInfo): String
    def infoString: String
  }

  object SimpleTermNameKind extends NameKind(UTF8) { self =>
    type ThisInfo = Info
    val info = new Info
    def mkString(underlying: TermName, info: ThisInfo) = unsupported("mkString")
    def infoString = unsupported("infoString")
  }

  abstract class ClassifiedNameKind(tag: Int, val infoString: String) extends NameKind(tag) {
    type ThisInfo = Info
    val info = new Info
    def apply(qual: TermName) =
      qual.derived(info)
    def unapply(name: DerivedTermName): Option[TermName] =  name match {
      case DerivedTermName(underlying, `info`) => Some(underlying)
      case _ => None
    }
    simpleNameKinds(tag) = this
  }

  class PrefixNameKind(tag: Int, prefix: String, optInfoString: String = "")
  extends ClassifiedNameKind(tag, if (optInfoString.isEmpty) s"Prefix $prefix" else optInfoString) {
    def mkString(underlying: TermName, info: ThisInfo) =
      underlying.mapLast(n => termName(prefix + n.toString)).toString
  }

  class SuffixNameKind(tag: Int, suffix: String, optInfoString: String = "")
  extends ClassifiedNameKind(tag, if (optInfoString.isEmpty) s"Suffix $suffix" else optInfoString) {
    def mkString(underlying: TermName, info: ThisInfo) = underlying.toString ++ suffix
  }

  trait QualifiedInfo extends NameInfo {
    val name: SimpleTermName
  }

  class QualifiedNameKind(tag: Int, val separator: String)
  extends NameKind(tag) {
    type ThisInfo = QualInfo
    case class QualInfo(val name: SimpleTermName) extends Info with QualifiedInfo {
      override def map(f: SimpleTermName => SimpleTermName): NameInfo = new QualInfo(f(name))
      override def toString = s"$infoString $name"
    }
    def apply(qual: TermName, name: SimpleTermName) =
      qual.derived(new QualInfo(name))
    def unapply(name: DerivedTermName): Option[(TermName, SimpleTermName)] = name match {
      case DerivedTermName(qual, info: this.QualInfo) => Some((qual, info.name))
      case _ => None
    }

    override def definesNewName = true

    def mkString(underlying: TermName, info: ThisInfo) =
      s"$underlying$separator${info.name}"
    def infoString = s"Qualified $separator"

    qualifiedNameKinds(separator) = this
  }

  object AnyQualifiedName {
    def unapply(name: DerivedTermName): Option[(TermName, SimpleTermName)] = name match {
      case DerivedTermName(qual, info: QualifiedInfo) =>
        Some((name.underlying, info.name))
      case _ => None
    }
  }

  trait NumberedInfo extends NameInfo {
    def num: Int
  }

  abstract class NumberedNameKind(tag: Int, val infoString: String) extends NameKind(tag) { self =>
    type ThisInfo = NumberedInfo
    case class NumberedInfo(val num: Int) extends Info with NameKinds.NumberedInfo {
      override def toString = s"$infoString $num"
    }
    def apply(qual: TermName, num: Int) =
      qual.derived(new NumberedInfo(num))
    def unapply(name: DerivedTermName): Option[(TermName, Int)] = name match {
      case DerivedTermName(underlying, info: this.NumberedInfo) => Some((underlying, info.num))
      case _ => None
    }
  }

  case class UniqueNameKind(val separator: String)
  extends NumberedNameKind(UNIQUE, s"Unique $separator") {
    override def definesNewName = true
    def mkString(underlying: TermName, info: ThisInfo) = {
      val safePrefix = str.sanitize(underlying.toString + separator)
      safePrefix + info.num
    }

    def fresh(prefix: TermName = EmptyTermName)(implicit ctx: Context): TermName =
      ctx.freshNames.newName(prefix, this)

    uniqueNameKinds(separator) = this
  }

  object AnyUniqueName {
    def unapply(name: DerivedTermName): Option[(TermName, String, Int)] = name match {
      case DerivedTermName(qual, info: NumberedInfo) =>
        info.kind match {
          case unique: UniqueNameKind => Some((qual, unique.separator, info.num))
          case _ => None
        }
      case _ => None
    }
  }

  val QualifiedName           = new QualifiedNameKind(QUALIFIED, ".")
  val FlattenedName           = new QualifiedNameKind(FLATTENED, "$")
  val ExpandedName            = new QualifiedNameKind(EXPANDED, str.EXPAND_SEPARATOR)
  val TraitSetterName         = new QualifiedNameKind(TRAITSETTER, str.TRAIT_SETTER_SEPARATOR)

  val UniqueName = new UniqueNameKind("$") {
    override def mkString(underlying: TermName, info: ThisInfo) =
      if (underlying.isEmpty) "$" + info.num + "$" else super.mkString(underlying, info)
  }

  val InlineAccessorName      = new UniqueNameKind("$_inlineAccessor_$")
  val TempResultName          = new UniqueNameKind("ev$")
  val EvidenceParamName       = new UniqueNameKind("evidence$")
  val DepParamName            = new UniqueNameKind("<param>")
  val LazyImplicitName        = new UniqueNameKind("$_lazy_implicit_$")
  val LazyLocalName           = new UniqueNameKind("$lzy")
  val LazyLocalInitName       = new UniqueNameKind("$lzyINIT")
  val LazyFieldOffsetName     = new UniqueNameKind("$OFFSET")
  val LazyBitMapName          = new UniqueNameKind(nme.BITMAP_PREFIX.toString)
  val NonLocalReturnKeyName   = new UniqueNameKind("nonLocalReturnKey")
  val WildcardParamName       = new UniqueNameKind("_$")
  val TailLabelName           = new UniqueNameKind("tailLabel")
  val ExceptionBinderName     = new UniqueNameKind("ex")
  val SkolemName              = new UniqueNameKind("?")
  val LiftedTreeName          = new UniqueNameKind("liftedTree")

  val PatMatStdBinderName     = new UniqueNameKind("x")
  val PatMatPiName            = new UniqueNameKind("pi") // FIXME: explain what this is
  val PatMatPName             = new UniqueNameKind("p")  // FIXME: explain what this is
  val PatMatOName             = new UniqueNameKind("o")  // FIXME: explain what this is
  val PatMatCaseName          = new UniqueNameKind("case")
  val PatMatMatchFailName     = new UniqueNameKind("matchFail")
  val PatMatSelectorName      = new UniqueNameKind("selector")

  object DefaultGetterName extends NumberedNameKind(DEFAULTGETTER, "DefaultGetter") {
    def mkString(underlying: TermName, info: ThisInfo) = {
      val prefix = if (underlying.isConstructorName) nme.DEFAULT_GETTER_INIT else underlying
      prefix.toString + nme.DEFAULT_GETTER + (info.num + 1)
    }
  }

  object VariantName extends NumberedNameKind(VARIANT, "Variant") {
    val varianceToPrefix = Map(-1 -> '-', 0 -> '=', 1 -> '+')
    val prefixToVariance = Map('-' -> -1, '=' -> 0, '+' -> 1)
    def mkString(underlying: TermName, info: ThisInfo) = {
      varianceToPrefix(info.num).toString + underlying
    }
  }

  val SuperAccessorName = new PrefixNameKind(SUPERACCESSOR, "super$")
  val InitializerName = new PrefixNameKind(INITIALIZER, "initial$")
  val ShadowedName = new PrefixNameKind(SHADOWED, "(shadowed)")
  val AvoidClashName = new SuffixNameKind(AVOIDCLASH, "$_avoid_name_clash_$")
  val ModuleClassName = new SuffixNameKind(OBJECTCLASS, "$", optInfoString = "ModuleClass")

  object SignedName extends NameKind(63) {

    /** @param parts  resultSig followed by paramsSig */
    case class SignedInfo(sig: Signature) extends Info {
      override def toString = s"$infoString $sig"
    }
    type ThisInfo = SignedInfo

    def apply(qual: TermName, sig: Signature) =
      qual.derived(new SignedInfo(sig))
    def unapply(name: DerivedTermName): Option[(TermName, Signature)] = name match {
      case DerivedTermName(underlying, info: SignedInfo) => Some((underlying, info.sig))
      case _ => None
    }

    def mkString(underlying: TermName, info: ThisInfo): String = unsupported("mkString")
    def infoString: String = "Signed"
  }

  def simpleNameKindOfTag         : collection.Map[Int, ClassifiedNameKind]   = simpleNameKinds
  def qualifiedNameKindOfSeparator: collection.Map[String, QualifiedNameKind] = qualifiedNameKinds
  def uniqueNameKindOfSeparator   : collection.Map[String, UniqueNameKind]    = uniqueNameKinds
}