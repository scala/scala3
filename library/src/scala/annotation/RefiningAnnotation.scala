package scala.annotation

/** A base trait for annotations that yield proper subtypes of the types they annotate.
 *  Refining annotations are more "sticky" than normal ones. They are conceptually kept
 *  around when normal refinements would also not be stripped away.
 */
trait RefiningAnnotation extends StaticAnnotation
