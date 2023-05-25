package de.sciss.kollflitz

import scala.collection.*

type Tagged[U]  = { type Tag  = U }
type @@ [+T, U] = T with Tagged[U]
private val anyTagger = new Tagger[Any]
final class Tagger[U] private[kollflitz] {
  def apply[T](t : T): T @@ U = t.asInstanceOf[T @@ U]
}
def tag[U]: Tagger[U] = anyTagger.asInstanceOf[Tagger[U]]

sealed trait Sorted


/** Enrichment methods for random access collections. */
implicit final class KollFlitzSortedIndexedSeq[A, CC[_], Repr](val self: SeqOps[A, CC, Repr] @@ Sorted)
  extends AnyVal {

  /** Nearest percentile (rounded index, no interpolation). */
  def percentile(n: Int): A = self((self.size * n - 50) / 100)

  /** Median found by rounding the index (no interpolation). */
  def median: A = percentile(50)
}
