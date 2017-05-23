import scala.collection.immutable.List
import scala.reflect.{ ClassTag, classTag }
import scala.language.implicitConversions

object Exception {
  type Catcher[+T] = PartialFunction[Throwable, T]

  def mkCatcher[Ex <: Throwable: ClassTag, T](isDef: Ex => Boolean, f: Ex => T) = new Catcher[T] {
    private def downcast(x: Throwable): Option[Ex] =
      if (classTag[Ex].runtimeClass.isAssignableFrom(x.getClass)) Some(x.asInstanceOf[Ex])
      else None

    def isDefinedAt(x: Throwable) = downcast(x) exists isDef
    def apply(x: Throwable): T = f(downcast(x).get)
  }

  def mkThrowableCatcher[T](isDef: Throwable => Boolean, f: Throwable => T) = mkCatcher(isDef, f) // error: undetermined ClassTag

  implicit def throwableSubtypeToCatcher[Ex <: Throwable: ClassTag, T](pf: PartialFunction[Ex, T]) = // error: result type needs to be given
    mkCatcher(pf.isDefinedAt _, pf.apply _)
}
