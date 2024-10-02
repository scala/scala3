package scala.annotation.internal

import scala.annotation.Annotation
import scala.annotation.experimental

/** Indicates the method was abstract in source code before unrolling transformation was added.
 *  downstream direct overrides, with matching `@unroll` annotations will infer an override.
 */
@experimental("under review as part of SIP-61")
final class AbstractUnroll extends Annotation
