package dotty.tools.dotc
package core

import Names._
import NameOps._
import StdNames._

/** Additional info associated with a name. At a minimum its kind and
 *  a way to turn it into a string.
 */
abstract class NameInfo extends util.DotClass {
  def kind: NameInfo.Kind
  def mkString(underlying: TermName): String
  def map(f: SimpleTermName => SimpleTermName): NameInfo = this
}

object NameInfo {

  type Kind = Int

  val TermNameKind = 0
  val QualifiedKind = 1
  val DefaultGetterKind = 3
  val VariantKind = 4
  val ModuleClassKind = 10

  val qualifier: Map[String, SimpleTermName => Qualified] =
    Map("."  -> Select,
        "$"  -> Flatten,
        str.EXPAND_SEPARATOR -> Expand,
        str.TRAIT_SETTER_SEPARATOR -> TraitSetter)

  def definesNewName(kind: Kind) = kind <= QualifiedKind

  /** TermNames have the lowest possible kind */
  val TermName = new NameInfo {
    def kind = TermNameKind
    def mkString(underlying: TermName) = underlying.toString // will cause an unsupported exception
  }

  trait Qualified extends NameInfo {
    def name: SimpleTermName
    def separator: String
    def newLikeThis(name: SimpleTermName): Qualified // TODO: should use copy instead after bootstrap

    def kind = QualifiedKind
    override def map(f: SimpleTermName => SimpleTermName): NameInfo = newLikeThis(f(name))
    def mkString(underlying: TermName) = s"$underlying$separator$name"
    override def toString = s"${getClass.getSimpleName}($name)"
  }

  case class Select(val name: SimpleTermName) extends Qualified {
    def separator = "."
    def newLikeThis(name: SimpleTermName) = Select(name)
  }

  case class Flatten(val name: SimpleTermName) extends Qualified {
    def separator = "$"
    def newLikeThis(name: SimpleTermName) = Flatten(name)
  }

  case class Expand(val name: SimpleTermName) extends Qualified {
    def separator = str.EXPAND_SEPARATOR
    def newLikeThis(name: SimpleTermName) = Expand(name)
  }

  case class TraitSetter(val name: SimpleTermName) extends Qualified {
    def separator = nme.TRAIT_SETTER_SEPARATOR.toString
    def newLikeThis(name: SimpleTermName) = TraitSetter(name)
  }

  trait Numbered extends NameInfo {
    def num: Int
    override def toString = s"${getClass.getSimpleName}($num)"
  }

  case class DefaultGetter(val num: Int) extends Numbered {
    def kind = DefaultGetterKind
    def mkString(underlying: TermName) = {
      val prefix = if (underlying.isConstructorName) nme.DEFAULT_GETTER_INIT else underlying
      prefix.toString + nme.DEFAULT_GETTER + (num + 1)
    }
  }

  case class Variant(val num: Int) extends Numbered {
    def kind = VariantKind
    def mkString(underlying: TermName) = varianceToPrefix(num).toString + underlying
  }

  val ModuleClass = new NameInfo {
    def kind = ModuleClassKind
    def mkString(underlying: TermName) = underlying + "$"
    override def toString = "ModuleClass"
  }

  /** Map between variances and name prefixes */
  val varianceToPrefix = Map(-1 -> '-', 0 -> '=', 1 -> '+')
  val prefixToVariance = Map('-' -> -1, '=' -> 0, '+' -> 1)
}