package dotty.tools.dotc
package core

import Names._

/** Additional info associated with a name. At a minimum its kind and
 *  a way to turn it into a string.
 */
abstract class NameInfo extends util.DotClass {
  def kind: NameInfo.Kind
  def mkString(underlying: TermName): String
  def contains(ch: Char): Boolean = false
  def ++(other: String): NameInfo = unsupported("++")
}

object NameInfo {

  type Kind = Int

  val TermNameKind = 0
  val QualifiedKind = 1
  val ModuleClassKind = 2

  def definesNewName(kind: Kind) = kind <= QualifiedKind

  /** TermNames have the lowest possible kind */
  val TermName = new NameInfo {
    def kind = TermNameKind
    def mkString(underlying: TermName) = underlying.toString // will cause an unsupported exception
  }

  case class Qualified(name: TermName, separator: String) extends NameInfo {
    def kind = QualifiedKind
    def mkString(underlying: TermName) = s"$underlying$separator$name"
    override def contains(ch: Char): Boolean = name.contains(ch)
    override def ++(other: String): NameInfo = Qualified(name ++ other, separator)
    override def toString = s"Qualified($name, $separator)"
  }

  val ModuleClass = new NameInfo {
    def kind = ModuleClassKind
    def mkString(underlying: TermName) = underlying + "$"
    override def toString = "ModuleClass"
  }
}