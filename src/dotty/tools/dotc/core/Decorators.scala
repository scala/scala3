package dotty.tools.dotc
package core

import annotation.tailrec
import Symbols._
import Contexts._, Names._, Phases._

/** This object provides useful implicit decorators for types defined elsewhere */
object Decorators {

  /** Turns Strings into PreNames, adding toType/TermName methods */
  implicit class StringDecorator(val s: String) extends AnyVal with PreName {
    def toTypeName: TypeName = typeName(s)
    def toTermName: TermName = termName(s)
    def toEncodedTypeName = encodedTypeName(s)
    def toEncodedTermName = encodedTermName(s)
  }

  /** Implements a findSymbol method on iterators of Symbols that
   *  works like find but avoids Option, replacing None with NoSymbol.
   */
  implicit class SymbolIteratorDecorator(val it: Iterator[Symbol]) extends AnyVal {
    final def findSymbol(p: Symbol => Boolean): Symbol = {
      while (it.hasNext) {
        val sym = it.next
        if (p(sym)) return sym
      }
      NoSymbol
    }
  }

  final val MaxFilterRecursions = 1000

  /** Implements a filterConserve method on lists that avoids
   *  duplication of list nodes if all tests yield true. Works for lists
   *  up to `MaxFilterRecursions` length.
   */
  implicit class ListDecorator[T](val xs: List[T]) extends AnyVal {

    /** Like `xs filter p` but returns list `xs` itself  - instead of a copy -
     *  if `p` is true for all elements and `xs` is not longer
     *  than `MaxFilterRecursions`.
     */
    def filterConserve(p: T => Boolean): List[T] = {
      def loop(xs: List[T], nrec: Int): List[T] = xs match {
        case Nil => xs
        case x :: xs1 =>
          if (nrec < MaxFilterRecursions) {
            val ys1 = loop(xs1, nrec + 1)
            if (p(x))
              if (ys1 eq xs1) xs else x :: ys1
            else
              ys1
          } else xs filter p
      }
      loop(xs, 0)
    }

    /** Like `(xs, ys).zipped.map(f)`, but returns list `xs` itself
     *  - instead of a copy - if function `f` maps all elements of
     *  `xs` to themselves. Also, it is required that `ys` is at least
     *  as long as `xs`.
     */
    def zipWithConserve[U](ys: List[U])(f: (T, U) => T): List[T] =
      if (xs.isEmpty) xs
      else {
        val x1 = f(xs.head, ys.head)
        val xs1 = xs.tail.zipWithConserve(ys.tail)(f)
        if ((x1.asInstanceOf[AnyRef] eq xs.head.asInstanceOf[AnyRef]) &&
            (xs1 eq xs.tail)) xs
        else x1 :: xs1
      }
  }

  /** Implements a test whether a list of strings representing phases contains
   *  a given phase. The test returns true if the given phase starts with
   *  one of the names in the list of strings.
   */
  implicit class PhaseListDecorator(val names: List[String]) extends AnyVal {
    def containsPhase(phase: Phase) =
      names exists (phase.name.startsWith)
  }
}

