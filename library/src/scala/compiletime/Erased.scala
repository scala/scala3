package scala.compiletime
import annotation.experimental

/** A marker trait for erased values. vals or parameters whose type extends
 *  `Erased` get an implicit `erased` modifier.
 */
@experimental trait Erased