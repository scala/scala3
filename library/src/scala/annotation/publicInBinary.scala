package scala.annotation

import language.experimental.captureChecking

/** A binary API is a definition that is annotated with `@publicInBinary`.
 *  This annotation can be placed on `def`, `val`, `lazy val`, `var`, class constructors, `object`, and `given` definitions.
 *  A binary API will be publicly available in the bytecode. Tools like TASTy MiMa will take this into account to check
 *  compatibility.
 *
 *  This annotation cannot be used on `private`/`private[this]` definitions.
 *
 *  `@publicInBinary` can be used to guarantee access to `private[T]`/`protected` definitions:
 *    - within inline definitions,
 *    - against previous binary where this definitions was public or less private,
 *    - or through JVM reflection.
 *
 *  Removing this annotation from a non-public definition is a binary incompatible change.
 *  Adding this annotation to a non-public definition can also cause binary incompatibilities
 *  if the definition is accessed in an inline definition (these can be checked using `-WunstableInlineAccessors`).
 */
final class publicInBinary extends scala.annotation.StaticAnnotation
