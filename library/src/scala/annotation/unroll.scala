package scala.annotation

import language.experimental.captureChecking

@experimental("under review as part of SIP-61")
/**The `@unroll` annotation is reserved for parameters of classes and methods.
 *
 * It enables to add new parameters while preserving backwards binary compatibility,
 * through code generation of hidden forwarder methods (but visible in the binary API).
 *
 * Read more about parameter unrolling, and the usage of `@unroll` in the reference documentation:
 * https://nightly.scala-lang.org/docs/reference/experimental/unrolled-defs.html
 */
final class unroll extends scala.annotation.StaticAnnotation
