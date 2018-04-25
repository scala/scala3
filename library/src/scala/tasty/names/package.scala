package scala.tasty

package object names {

  object Simple {
    type Data = String
    def unapply(arg: TermName)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplySimple(arg)
  }

  // s"$prefix.$name"
  object Qualified {
    type Data = (TermName, String)
    def unapply(arg: TermName)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyQualified(arg)
  }

  // s"$methodName${"$default$"}${idx+1}"
  object DefaultGetter {
    type Data = (TermName, String)
    def unapply(arg: TermName)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyDefaultGetter(arg)
  }

  // s"${if (covariant) "+" else "-"}$underlying"
  object Variant {
    type Data = (TermName, Boolean)
    def unapply(arg: TermName)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyVariant(arg)
  }

  // s"${"super$"}$underlying"
  object SuperAccessor {
    type Data = TermName
    def unapply(arg: TermName)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplySuperAccessor(arg)
  }

  // s"${"protected$"}$underlying"
  object ProtectedAccessor {
    type Data = TermName
    def unapply(arg: TermName)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyProtectedAccessor(arg)
  }

  // s"${"protected$set"}$underlying"
  object ProtectedSetter {
    type Data = TermName
    def unapply(arg: TermName)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyProtectedSetter(arg)
  }

  // s"$underlying${"$"}"
  object ObjectClass {
    type Data = TermName
    def unapply(arg: TermName)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyObjectClass(arg)
  }

}
