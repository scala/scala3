package scala.compiletime
import annotation.experimental

import language.experimental.captureChecking

/** A marker trait for erased values. vals or parameters whose type extends
 *  `Erased` get an implicit `erased` modifier.
 */
@experimental trait Erased
