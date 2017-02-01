package scala.reflect

class Selectable(val receiver: Any) extends AnyVal with scala.Selectable {
  def selectDynamic(name: String): Any = {
    val rcls = receiver.getClass
    try {
      val fld = rcls.getField(name)
      fld.get(receiver)
    }
    catch {
      case ex: NoSuchFieldError =>
        selectDynamicMethod(name).asInstanceOf[() => Any]()
    }
  }

  override def selectDynamicMethod(name: String, paramTypes: ClassTag[_]*): Any = {
    val rcls = receiver.getClass
    val paramClasses = paramTypes.map(_.runtimeClass)
    val mth = rcls.getMethod(name, paramClasses: _*)
    paramTypes.length match {
      case 0 => () =>
        mth.invoke(receiver)
      case 1 => (x0: Any) =>
        mth.invoke(receiver, x0.asInstanceOf[Object])
      case 2 => (x0: Any, x1: Any) =>
        mth.invoke(receiver,
            x0.asInstanceOf[Object],
            x1.asInstanceOf[Object])
      case 3 => (x0: Any, x1: Any, x2: Any) =>
        mth.invoke(receiver,
            x0.asInstanceOf[Object],
            x1.asInstanceOf[Object],
            x2.asInstanceOf[Object])
      case 4 => (x0: Any, x1: Any, x2: Any, x3: Any) =>
        mth.invoke(receiver,
            x0.asInstanceOf[Object],
            x1.asInstanceOf[Object],
            x2.asInstanceOf[Object],
            x3.asInstanceOf[Object])
      case 5 => (x0: Any, x1: Any, x2: Any, x3: Any, x4: Any) =>
        mth.invoke(receiver,
            x0.asInstanceOf[Object],
            x1.asInstanceOf[Object],
            x2.asInstanceOf[Object],
            x3.asInstanceOf[Object],
            x4.asInstanceOf[Object])
      case 6 => (x0: Any, x1: Any, x2: Any, x3: Any, x4: Any, x5: Any) =>
        mth.invoke(receiver,
            x0.asInstanceOf[Object],
            x1.asInstanceOf[Object],
            x2.asInstanceOf[Object],
            x3.asInstanceOf[Object],
            x4.asInstanceOf[Object],
            x5.asInstanceOf[Object])
      case 7 => (x0: Any, x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any) =>
        mth.invoke(receiver,
            x0.asInstanceOf[Object],
            x1.asInstanceOf[Object],
            x2.asInstanceOf[Object],
            x3.asInstanceOf[Object],
            x4.asInstanceOf[Object],
            x5.asInstanceOf[Object],
            x6.asInstanceOf[Object])
      }
  }
}

object Selectable {
  implicit def reflectiveSelectable(receiver: Any): scala.Selectable = receiver match {
    case receiver: scala.Selectable => receiver
    case _                          => new Selectable(receiver)
  }
}
