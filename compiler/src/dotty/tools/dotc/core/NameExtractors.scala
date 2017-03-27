package dotty.tools.dotc
package core

import Names._
import NameOps._
import StdNames._
import util.DotClass
import tasty.TastyFormat._
import Decorators._
import collection.mutable

object NameExtractors {

  private val extractors = new mutable.HashMap[Int, ClassifiedNameExtractor]

  abstract class NameInfo extends DotClass {
    def tag: Int
    def mkString(underlying: TermName): String
    def map(f: SimpleTermName => SimpleTermName): NameInfo = this
  }

  val simpleTermNameInfo = new NameInfo {
    def tag = UTF8
    def mkString(underlying: TermName): String = unsupported("mkString")
  }

  abstract class NameExtractor(val tag: Int) extends DotClass { self =>
    def mkString(underlying: TermName, info: ThisInfo): String
    def infoString: String
    type ThisInfo <: Info
    class Info extends NameInfo { this: ThisInfo =>
      def tag = self.tag
      def mkString(underlying: TermName) = self.mkString(underlying, this)
      override def toString = infoString
    }
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
    extractors(tag) = this
  }

  class PrefixNameExtractor(tag: Int, prefix: String, infoString: String) extends ClassifiedNameExtractor(tag, infoString) {
    def mkString(underlying: TermName, info: ThisInfo) =
      underlying.mapLast(n => termName(prefix + n.toString)).toString
  }

  class SuffixNameExtractor(tag: Int, suffix: String, infoString: String) extends ClassifiedNameExtractor(tag, infoString) {
    def mkString(underlying: TermName, info: ThisInfo) = underlying.toString ++ suffix
  }

  trait QualifiedInfo extends NameInfo {
    val name: SimpleTermName
  }

  abstract class QualifiedNameExtractor(tag: Int, val separator: String, val infoString: String) extends NameExtractor(tag) {
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
    def mkString(underlying: TermName, info: ThisInfo) =
      s"$underlying$separator${info.name}"
  }

  object AnyQualifiedName {
    def unapply(name: DerivedTermName): Option[(TermName, QualifiedInfo)] = name match {
      case DerivedTermName(qual, info: QualifiedInfo) =>
        Some((name.underlying, info))
      case _ => None
    }
  }

  trait NumberedInfo {
    def num: Int
  }

  abstract class NumberedNameExtractor(tag: Int, val infoString: String) extends NameExtractor(tag) {
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

  object QualifiedName   extends QualifiedNameExtractor(QUALIFIED, ".", "Qualified")
  object FlattenedName   extends QualifiedNameExtractor(FLATTENED, "$", "Flattened")
  object ExpandedName    extends QualifiedNameExtractor(EXPANDED, str.EXPAND_SEPARATOR, "Expanded")
  object TraitSetterName extends QualifiedNameExtractor(TRAITSETTER, str.TRAIT_SETTER_SEPARATOR, "TraitSetter")

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

  val SuperAccessorName = new PrefixNameExtractor(SUPERACCESSOR, str.SUPER_PREFIX, "SuperAccessor")
  val InitializerName = new PrefixNameExtractor(INITIALIZER, str.INITIALIZER_PREFIX, "Initializer")
  val ShadowedName = new PrefixNameExtractor(SHADOWED, str.SHADOWED_PREFIX, "Shadowed")
  val ModuleClassName = new SuffixNameExtractor(OBJECTCLASS, "$", "ModuleClass")

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

  def definesNewName(tag: Int) = tag <= TraitSetterName.tag

  def extractorOfTag(tag: Int) = extractors(tag)

  val separatorToQualified: Map[String, QualifiedNameExtractor] =
    Map("." -> QualifiedName,
      "$" -> FlattenedName,
      str.EXPAND_SEPARATOR -> ExpandedName,
      str.TRAIT_SETTER_SEPARATOR -> TraitSetterName)
}