package scala.quoted.runtime

import language.experimental.captureChecking

import scala.annotation.{Annotation, compileTimeOnly}

/** Artifact of pickled type splices
 *
 *  During quote reification a quote `'{ ... F[t.Underlying] ... }` will be transformed into
 *  `'{ @SplicedType type T$1 = t.Underlying ... F[T$1] ... }` to have a tree for `t.Underlying`.
 *  This artifact is removed during quote unpickling.
 *
 *  See PickleQuotes.scala and PickledQuotes.scala
 *
 *  Adding this annotation in source has undefined behavior at compile-time
 */
@compileTimeOnly("Illegal reference to `scala.quoted.runtime.SplicedType`")
class SplicedType extends Annotation
