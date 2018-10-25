object Test {
  import java.util.{ concurrent => juc }
  import scala.collection.concurrent
  import scala.collection.convert.Wrappers._

  /**
   * Implicitly converts a Java ConcurrentMap to a Scala mutable ConcurrentMap.
   * The returned Scala ConcurrentMap is backed by the provided Java
   * ConcurrentMap and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   *
   * If the Java ConcurrentMap was previously obtained from an implicit or
   * explicit call of `asConcurrentMap(scala.collection.mutable.ConcurrentMap)`
   * then the original Scala ConcurrentMap will be returned.
   *
   * @param m The ConcurrentMap to be converted.
   * @return A Scala mutable ConcurrentMap view of the argument.
   */
  implicit def mapAsScalaConcurrentMap[A, B](m: juc.ConcurrentMap[A, B]|Null): concurrent.Map[A, B]|Null = m match {
    case null                             => null
    case cmw: ConcurrentMapWrapper[_, _]  => cmw.underlying
    case cm: juc.ConcurrentMap[A, B]      => new JConcurrentMapWrapper(cm)
  }
}
