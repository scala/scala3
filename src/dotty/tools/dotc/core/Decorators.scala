package dotty.tools.dotc
package core

import Contexts._, Names._

object Decorators {

  implicit class toTypeNameDecorator(val s: String) extends AnyVal {
    def toTypeName(implicit context: Context): TypeName =
      context.names.newTypeName(s)
  }

  implicit class toTermNameDecorator(val s: String) extends AnyVal {
    def toTermName(implicit context: Context): TermName =
      context.names.newTermName(s)
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

