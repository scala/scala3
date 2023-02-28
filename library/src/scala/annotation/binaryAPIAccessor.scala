package scala.annotation

/** A binary API with accessor is a definition that is annotated with `@binaryAPIAccessor`.
 *  This annotation can be placed on `def`, `val`, `lazy val`, `var`, `object`, and `given` definitions.
 *  The annotated definition will get a public accessor.
 *
 *  This can be used to access `private`/`private[this]` definitions within inline definitions.
 *  To access other private/protected definition see `scala.annotation.binaryAPI`.
 *
 *  Removing this annotation is a binary incompatible change.
 */
final class binaryAPIAccessor extends scala.annotation.StaticAnnotation
