package scala.annotation

/** A binary API is a definition that is annotated with `@binaryAPI` or overrides a definition annotated with `@binaryAPI`.
 *  This annotation can be placed on `def`, `val`, `lazy val`, `var`, `object`, and `given` definitions.
 *  A binary API will be publicly available in the bytecode.
 *
 *  - `private`/`private[this]` definitions will get an accessor.
 *  - `private[T]` and `protected` definitions will become public in the bytecode.
 */
final class binaryAPI extends scala.annotation.StaticAnnotation
