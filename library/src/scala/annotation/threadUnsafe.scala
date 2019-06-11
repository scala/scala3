package scala.annotation

/** This annotation can only be used on a field which defines a lazy val.
 *  When this annotation is used, the initialization of the lazy val will use a
 *  faster mechanism which is not thread-safe.
 */
final class threadUnsafe extends StaticAnnotation
