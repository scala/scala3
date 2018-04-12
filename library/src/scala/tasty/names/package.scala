package scala.tasty

package object names {

  object Simple {
    type Data = String
    def unapply(arg: Name /* | PossiblySigned */)(implicit ext: Extractor): Option[Data] = ext.unapplySimple(arg)
  }

  // s"$prefix.$name"
  object Qualified {
    type Data = (TermName, String)
    def unapply(arg: Name /* | PossiblySigned */)(implicit ext: Extractor): Option[Data] = ext.unapplyQualified(arg)
  }

  // s"$methodName${"$default$"}${idx+1}"
  object DefaultGetter {
    type Data = (TermName, String)
    def unapply(arg: Name /* | PossiblySigned */)(implicit ext: Extractor): Option[Data] = ext.unapplyDefaultGetter(arg)
  }

  // s"${if (covariant) "+" else "-"}$underlying"
  object Variant {
    type Data = (TermName, Boolean)
    def unapply(arg: Name /* | PossiblySigned */)(implicit ext: Extractor): Option[Data] = ext.unapplyVariant(arg)
  }

  // s"${"super$"}$underlying"
  object SuperAccessor {
    type Data = TermName
    def unapply(arg: Name /* | PossiblySigned */)(implicit ext: Extractor): Option[Data] = ext.unapplySuperAccessor(arg)
  }

  // s"${"protected$"}$underlying"
  object ProtectedAccessor {
    type Data = TermName
    def unapply(arg: Name /* | PossiblySigned */)(implicit ext: Extractor): Option[Data] = ext.unapplyProtectedAccessor(arg)
  }

  // s"${"protected$set"}$underlying"
  object ProtectedSetter {
    type Data = TermName
    def unapply(arg: Name /* | PossiblySigned */)(implicit ext: Extractor): Option[Data] = ext.unapplyProtectedSetter(arg)
  }

  // s"$underlying${"$"}"
  object ObjectClass {
    type Data = TermName
    def unapply(arg: Name /* | PossiblySigned */)(implicit ext: Extractor): Option[Data] = ext.unapplyObjectClass(arg)
  }

}
