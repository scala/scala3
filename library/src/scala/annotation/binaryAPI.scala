package scala.annotation

/** A binary API is a definition that is annotated with `@binaryAPI` or overrides a definition annotated with `@binaryAPI`.
 *  This annotation can be placed on `def`, `val`, `lazy val`, `var`, `object`, and `given` definitions.
 *  A binary API will be publicly available in the bytecode.
 *
 *  This annotation cannot be used on `private`/`private[this]` definitions. See `scala.annotation.binaryAPIAccessor`.
 *
 *  This can be used to access private/protected definitions within inline definitions.
 *
 *  Removing this annotation from a non-public definition is a binary incompatible change.
 */
final class binaryAPI extends scala.annotation.StaticAnnotation
