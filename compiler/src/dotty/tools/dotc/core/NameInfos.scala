package dotty.tools.dotc
package core

import Names._

/** Additional info associated with a name. At a minimum its kind and
 *  a way to turn it into a string.
 */
abstract class NameInfo {
  def kind: NameInfo.Kind
  def oneOfAKind = true
  def mkString(underlying: TermName): String
}

object NameInfo {

  type Kind = Int

  val TermNameKind = 0
  val QualifiedKind = 1

  /** TermNames have the lowest possible kind */
  val TermName = new NameInfo {
    def kind = 0
    def mkString(underlying: TermName) = underlying.toString // will cause an unsupptored exception
  }

  case class Qualified(name: TermName) extends NameInfo {
    def kind = 1
    override def oneOfAKind = false
    def mkString(underlying: TermName) = s"$underlying.$name"
  }

  val ModuleClass = new NameInfo {
    def kind = 10
    def mkString(underlying: TermName) = underlying + "$"
  }

}