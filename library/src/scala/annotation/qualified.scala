package scala.annotation

/** Annotation for qualified types. */
@experimental class qualified[T](predicate: T => Boolean) extends RefiningAnnotation
