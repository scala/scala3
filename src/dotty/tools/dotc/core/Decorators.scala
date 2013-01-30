package dotty.tools.dotc
package core

import annotation.tailrec
import Symbols._

import Contexts._, Names._

object Decorators {

  implicit class StringDecorator(val s: String) extends AnyVal {
    def toTypeName: TypeName = typeName(s)
    def toTermName: TermName = termName(s)
    def toEncodedTypeName = encodedTypeName(s)
    def toEncodedTermName = encodedTermName(s)
  }

  implicit class SymbolIteratorDecorator(val it: Iterator[Symbol]) extends AnyVal {
    final def findSymbol(p: Symbol => Boolean): Symbol = {
      while (it.hasNext) {
        val sym = it.next
        if (p(sym)) return sym
      }
      NoSymbol
    }
  }

  final val MaxRecursions = 1000

  implicit class ListDecorator[T](val xs: List[T]) extends AnyVal {
    def filterConserve(p: T => Boolean): List[T] = {
      def loop(xs: List[T], nrec: Int): List[T] = xs match {
        case Nil => xs
        case x :: xs1 =>
          if (nrec < MaxRecursions) {
            val ys1 = loop(xs1, nrec + 1)
            if (p(x))
              if (ys1 eq xs1) xs else x :: ys1
            else
              ys1
          } else xs filter p
      }
      loop(xs, 0)
    }
  }
}

