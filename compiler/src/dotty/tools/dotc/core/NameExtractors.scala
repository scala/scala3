package dotty.tools.dotc
package core

import Names._
import NameOps._
import StdNames._
import util.DotClass

object NameExtractors {

  abstract class NameInfo extends DotClass {
    def tag: Int
    def mkString(underlying: TermName): String
    def map(f: SimpleTermName => SimpleTermName): NameInfo = this
  }

  val simpleTermNameInfo = new NameInfo {
    def tag = 0
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
  }

  class PrefixNameExtractor(tag: Int, prefix: String, infoString: String) extends ClassifiedNameExtractor(tag, infoString) {
    def mkString(underlying: TermName, info: ThisInfo) = prefix ++ underlying
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
    def unapply(name: DerivedTermName): Option[(TermName, QualifiedNameExtractor # QualInfo)] = name match {
      case DerivedTermName(qual, info: QualifiedNameExtractor # QualInfo) =>
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

  object QualifiedName   extends QualifiedNameExtractor(1, ".", "Qualified")
  object FlattenedName   extends QualifiedNameExtractor(2, "$", "Flattened")
  object XpandedName    extends QualifiedNameExtractor(3, str.EXPAND_SEPARATOR, "Expanded")
  object TraitSetterName extends QualifiedNameExtractor(4, str.TRAIT_SETTER_SEPARATOR, "TraitSetter")

  object DefaultGetterName extends NumberedNameExtractor(5, "DefaultGetter") {
    def mkString(underlying: TermName, info: ThisInfo) = {
      val prefix = if (underlying.isConstructorName) nme.DEFAULT_GETTER_INIT else underlying
      prefix.toString + nme.DEFAULT_GETTER + (info.num + 1)
    }
  }

  object VariantName extends NumberedNameExtractor(6, "Variant") {
    val varianceToPrefix = Map(-1 -> '-', 0 -> '=', 1 -> '+')
    val prefixToVariance = Map('-' -> -1, '=' -> 0, '+' -> 1)
    def mkString(underlying: TermName, info: ThisInfo) = {
      varianceToPrefix(info.num).toString + underlying
    }
  }

  val SuperAccessorName = new PrefixNameExtractor(7, str.SUPER_PREFIX, "SuperAccessor")
  val InitializerName = new PrefixNameExtractor(8, str.INITIALIZER_PREFIX, "Initializer")
  val ShadowedName = new PrefixNameExtractor(9, str.SHADOWED_PREFIX, "Shadowed")
  val ModuleClassName = new SuffixNameExtractor(10, "$", "ModuleClass")

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

  val separatorToQualified: Map[String, QualifiedNameExtractor] =
    Map("." -> QualifiedName,
      "$" -> FlattenedName,
      str.EXPAND_SEPARATOR -> XpandedName,
      str.TRAIT_SETTER_SEPARATOR -> TraitSetterName)
}