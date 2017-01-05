package scala.reflect

class Projector extends scala.Projector[Any] {
  import Projector._
  def get(receiver: Any, name: String): Any = {
    val rcls = receiver.getClass
    try {
      val fld = rcls.getField(name)
      fld.get(receiver)
    }
    catch {
      case ex: NoSuchFieldError =>
        getMethod(receiver, name).asInstanceOf[() => Any]()
    }
  }

  override def getMethod(receiver: Any, name: String, paramTypes: ClassTag[_]*): Any = {
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

object Projector {
  implicit def reflectiveProjector: scala.Projector[Any] = new Projector
}