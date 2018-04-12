package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.core.Names

import scala.tasty.names

object TermName {

  def apply(name: Names.TermName): names.TermName = Impl(name)


  object Simple {
    def unapply(name: names.Name): Option[String] = name match {
      case Impl(name: Names.SimpleName) => Some(name.toString)
      case _ => None
    }
  }

//  case Qualified(prefix: TermName, selector: String)              // s"$prefix.$name"
//
//  case DefaultGetter(methodName: TermName, idx: String)           // s"$methodName${"$default$"}${idx+1}"
//  case Variant(underlying: TermName, covariant: Boolean)          // s"${if (covariant) "+" else "-"}$underlying"
//  case SuperAccessor(underlying: TermName)                        // s"${"super$"}$underlying"
//  case ProtectedAccessor(underlying: TermName)                    // s"${"protected$"}$underlying"
//  case ProtectedSetter(underlying: TermName)                      // s"${"protected$set"}$underlying"
//  case ObjectClass(underlying: TermName)                          // s"$underlying${"$"}"

  private[tasty] case class Impl(name: Names.TermName) extends names.TermName {
    override def toString: String = name.toString
  }

}
