package scala.tasty

import scala.runtime.tasty.Toolbox

package object names {

  object Simple {
    type Data = String
    def unapply(arg: Name /* | PossiblySigned */)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplySimple(arg)
  }

  // s"$prefix.$name"
  object Qualified {
    type Data = (TermName, String)
    def unapply(arg: Name /* | PossiblySigned */)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyQualified(arg)
  }

  // s"$methodName${"$default$"}${idx+1}"
  object DefaultGetter {
    type Data = (TermName, String)
    def unapply(arg: Name /* | PossiblySigned */)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyDefaultGetter(arg)
  }

  // s"${if (covariant) "+" else "-"}$underlying"
  object Variant {
    type Data = (TermName, Boolean)
    def unapply(arg: Name /* | PossiblySigned */)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyVariant(arg)
  }

  // s"${"super$"}$underlying"
  object SuperAccessor {
    type Data = TermName
    def unapply(arg: Name /* | PossiblySigned */)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplySuperAccessor(arg)
  }

  // s"${"protected$"}$underlying"
  object ProtectedAccessor {
    type Data = TermName
    def unapply(arg: Name /* | PossiblySigned */)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyProtectedAccessor(arg)
  }

  // s"${"protected$set"}$underlying"
  object ProtectedSetter {
    type Data = TermName
    def unapply(arg: Name /* | PossiblySigned */)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyProtectedSetter(arg)
  }

  // s"$underlying${"$"}"
  object ObjectClass {
    type Data = TermName
    def unapply(arg: Name /* | PossiblySigned */)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyObjectClass(arg)
  }

}
