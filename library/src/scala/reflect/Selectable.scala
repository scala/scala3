package scala.reflect

class Selectable(val receiver: Any) extends AnyVal with scala.Selectable {
  def selectDynamic(name: String): Any = {
    val rcls = receiver.getClass
    try {
      val fld = rcls.getField(name)
      ensureAccessible(fld)
      fld.get(receiver)
    }
    catch {
      case ex: NoSuchFieldException =>
        applyDynamic(name)()
    }
  }

  override def applyDynamic(name: String, paramTypes: ClassTag[_]*)(args: Any*): Any = {
    val rcls = receiver.getClass
    val paramClasses = paramTypes.map(_.runtimeClass)
    val mth = rcls.getMethod(name, paramClasses: _*)
    ensureAccessible(mth)
    mth.invoke(receiver, args.map(_.asInstanceOf[AnyRef]): _*)
  }
}

object Selectable {
  implicit def reflectiveSelectable(receiver: Any): scala.Selectable = receiver match {
    case receiver: scala.Selectable => receiver
    case _                          => new Selectable(receiver)
  }
}
