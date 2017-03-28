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

object NameExtractors {

  @sharable private val simpleExtractors = new mutable.HashMap[Int, ClassifiedNameExtractor]
  @sharable private val uniqueExtractors = new mutable.HashMap[String, UniqueNameExtractor]
  @sharable private val qualifiedExtractors = new mutable.HashMap[String, QualifiedNameExtractor]

  abstract class NameInfo extends DotClass {
    def extractor: NameExtractor
    def mkString(underlying: TermName): String
    def map(f: SimpleTermName => SimpleTermName): NameInfo = this
  }

  abstract class NameExtractor(val tag: Int) extends DotClass { self =>
    type ThisInfo <: Info
    class Info extends NameInfo { this: ThisInfo =>
      def extractor = self
      def mkString(underlying: TermName) = self.mkString(underlying, this)
      override def toString = infoString
    }
    def definesNewName = false
    def mkString(underlying: TermName, info: ThisInfo): String
    def infoString: String
  }

  object SimpleTermNameExtractor extends NameExtractor(UTF8) { self =>
    type ThisInfo = Info
    val info = new Info
    def mkString(underlying: TermName, info: ThisInfo) = unsupported("mkString")
    def infoString = unsupported("infoString")
  }

  abstract class ClassifiedNameExtractor(tag: Int, val infoString: String) extends NameExtractor(tag) {
    type ThisInfo = Info
    val info = new Info
    def apply(qual: TermName) =
      qual.derived(info)
    def unapply(name: DerivedTermName): Option[TermName] =  name match {
      case DerivedTermName(underlying, `info`) => Some(underlying)
      case _ => None
    }
    simpleExtractors(tag) = this
  }

  class PrefixNameExtractor(tag: Int, prefix: String, optInfoString: String = "")
  extends ClassifiedNameExtractor(tag, if (optInfoString.isEmpty) s"Prefix $prefix" else optInfoString) {
    def mkString(underlying: TermName, info: ThisInfo) =
      underlying.mapLast(n => termName(prefix + n.toString)).toString
  }

  class SuffixNameExtractor(tag: Int, suffix: String, optInfoString: String = "")
  extends ClassifiedNameExtractor(tag, if (optInfoString.isEmpty) s"Suffix $suffix" else optInfoString) {
    def mkString(underlying: TermName, info: ThisInfo) = underlying.toString ++ suffix
  }

  trait QualifiedInfo extends NameInfo {
    val name: SimpleTermName
  }

  class QualifiedNameExtractor(tag: Int, val separator: String)
  extends NameExtractor(tag) {
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

    qualifiedExtractors(separator) = this
  }

  object AnyQualifiedName {
    def unapply(name: DerivedTermName): Option[(TermName, SimpleTermName)] = name match {
      case DerivedTermName(qual, info: QualifiedInfo) =>
        Some((name.underlying, info.name))
      case _ => None
    }
  }

  trait NumberedInfo {
    def num: Int
    def extractor: NameExtractor
  }

  abstract class NumberedNameExtractor(tag: Int, val infoString: String) extends NameExtractor(tag) { self =>
    type ThisInfo = NumberedInfo
    case class NumberedInfo(val num: Int) extends Info with NameExtractors.NumberedInfo {
      override def toString = s"$infoString $num"
    }
    def apply(qual: TermName, num: Int) =
      qual.derived(new NumberedInfo(num))
    def unapply(name: DerivedTermName): Option[(TermName, Int)] = name match {
      case DerivedTermName(underlying, info: this.NumberedInfo) => Some((underlying, info.num))
      case _ => None
    }
  }

  case class UniqueNameExtractor(val separator: String)
  extends NumberedNameExtractor(UNIQUE, s"Unique $separator") {
    override def definesNewName = true
    def mkString(underlying: TermName, info: ThisInfo) = {
      val safePrefix = str.sanitize(underlying.toString + separator)
      safePrefix + info.num
    }

    def fresh(prefix: TermName = EmptyTermName)(implicit ctx: Context): TermName =
      ctx.freshNames.newName(prefix, this)

    uniqueExtractors(separator) = this
  }

  object AnyUniqueName {
    def unapply(name: DerivedTermName): Option[(TermName, String, Int)] = name match {
      case DerivedTermName(qual, info: NumberedInfo) =>
        info.extractor match {
          case unique: UniqueNameExtractor => Some((qual, unique.separator, info.num))
          case _ => None
        }
      case _ => None
    }
  }

  val QualifiedName           = new QualifiedNameExtractor(QUALIFIED, ".")
  val FlattenedName           = new QualifiedNameExtractor(FLATTENED, "$")
  val ExpandedName            = new QualifiedNameExtractor(EXPANDED, str.EXPAND_SEPARATOR)
  val TraitSetterName         = new QualifiedNameExtractor(TRAITSETTER, str.TRAIT_SETTER_SEPARATOR)

  val UniqueName = new UniqueNameExtractor("$") {
    override def mkString(underlying: TermName, info: ThisInfo) =
      if (underlying.isEmpty) "$" + info.num + "$" else super.mkString(underlying, info)
  }

  val InlineAccessorName      = new UniqueNameExtractor("$_inlineAccessor_$")
  val TempResultName          = new UniqueNameExtractor("ev$")
  val EvidenceParamName       = new UniqueNameExtractor("evidence$")
  val DepParamName            = new UniqueNameExtractor("<param>")
  val LazyImplicitName        = new UniqueNameExtractor("$_lazy_implicit_$")
  val LazyLocalName           = new UniqueNameExtractor("$lzy")
  val LazyLocalInitName       = new UniqueNameExtractor("$lzyINIT")
  val LazyFieldOffsetName     = new UniqueNameExtractor("$OFFSET")
  val LazyBitMapName          = new UniqueNameExtractor(nme.BITMAP_PREFIX.toString)
  val NonLocalReturnKeyName   = new UniqueNameExtractor("nonLocalReturnKey")
  val WildcardParamName       = new UniqueNameExtractor("_$")
  val TailLabelName           = new UniqueNameExtractor("tailLabel")
  val ExceptionBinderName     = new UniqueNameExtractor("ex")
  val SkolemName              = new UniqueNameExtractor("?")
  val LiftedTreeName          = new UniqueNameExtractor("liftedTree")

  val PatMatStdBinderName     = new UniqueNameExtractor("x")
  val PatMatPiName            = new UniqueNameExtractor("pi") // FIXME: explain what this is
  val PatMatPName             = new UniqueNameExtractor("p")  // FIXME: explain what this is
  val PatMatOName             = new UniqueNameExtractor("o")  // FIXME: explain what this is
  val PatMatCaseName          = new UniqueNameExtractor("case")
  val PatMatMatchFailName     = new UniqueNameExtractor("matchFail")
  val PatMatSelectorName      = new UniqueNameExtractor("selector")

  object DefaultGetterName extends NumberedNameExtractor(DEFAULTGETTER, "DefaultGetter") {
    def mkString(underlying: TermName, info: ThisInfo) = {
      val prefix = if (underlying.isConstructorName) nme.DEFAULT_GETTER_INIT else underlying
      prefix.toString + nme.DEFAULT_GETTER + (info.num + 1)
    }
  }

  object VariantName extends NumberedNameExtractor(VARIANT, "Variant") {
    val varianceToPrefix = Map(-1 -> '-', 0 -> '=', 1 -> '+')
    val prefixToVariance = Map('-' -> -1, '=' -> 0, '+' -> 1)
    def mkString(underlying: TermName, info: ThisInfo) = {
      varianceToPrefix(info.num).toString + underlying
    }
  }

  val SuperAccessorName = new PrefixNameExtractor(SUPERACCESSOR, "super$")
  val InitializerName = new PrefixNameExtractor(INITIALIZER, "initial$")
  val ShadowedName = new PrefixNameExtractor(SHADOWED, "(shadowed)")
  val AvoidClashName = new SuffixNameExtractor(AVOIDCLASH, "$_avoid_name_clash_$")
  val ModuleClassName = new SuffixNameExtractor(OBJECTCLASS, "$", optInfoString = "ModuleClass")

  object SignedName extends NameExtractor(63) {

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

  def simpleExtractorOfTag         : collection.Map[Int, ClassifiedNameExtractor]   = simpleExtractors
  def qualifiedExtractorOfSeparator: collection.Map[String, QualifiedNameExtractor] = qualifiedExtractors
  def uniqueExtractorOfSeparator   : collection.Map[String, UniqueNameExtractor]    = uniqueExtractors
}