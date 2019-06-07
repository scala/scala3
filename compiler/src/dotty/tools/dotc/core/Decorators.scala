package dotty.tools.dotc
package core

import annotation.tailrec
import Symbols._
import Contexts._, Names._, Phases._, printing.Texts._, printing.Printer
import util.Spans.Span, util.SourcePosition
import collection.mutable.ListBuffer
import dotty.tools.dotc.transform.MegaPhase
import ast.tpd._
import scala.language.implicitConversions
import printing.Formatting._

/** This object provides useful implicit decorators for types defined elsewhere */
object Decorators {

  /** Turns Strings into PreNames, adding toType/TermName methods */
  implicit class PreNamedString(val s: String) extends AnyVal with PreName {
    def toTypeName: TypeName = typeName(s)
    def toTermName: TermName = termName(s)
    def toText(printer: Printer): Text = Str(s)
  }

  implicit class StringDecorator(val s: String) extends AnyVal {
    def splitWhere(f: Char => Boolean, doDropIndex: Boolean): Option[(String, String)] = {
      def splitAt(idx: Int, doDropIndex: Boolean): Option[(String, String)] =
        if (idx == -1) None
        else Some((s.take(idx), s.drop(if (doDropIndex) idx + 1 else idx)))

      splitAt(s.indexWhere(f), doDropIndex)
    }
  }

  /** Implements a findSymbol method on iterators of Symbols that
   *  works like find but avoids Option, replacing None with NoSymbol.
   */
  implicit class SymbolIteratorDecorator(val it: Iterator[Symbol]) extends AnyVal {
    final def findSymbol(p: Symbol => Boolean): Symbol = {
      while (it.hasNext) {
        val sym = it.next()
        if (p(sym)) return sym
      }
      NoSymbol
    }
  }

  final val MaxFilterRecursions = 1000

  /** Implements filterConserve, zipWithConserve methods
   *  on lists that avoid duplication of list nodes where feasible.
   */
  implicit class ListDecorator[T](val xs: List[T]) extends AnyVal {

    final def mapconserve[U](f: T => U): List[U] = {
      @tailrec
      def loop(mapped: ListBuffer[U], unchanged: List[U], pending: List[T]): List[U] =
        if (pending.isEmpty) {
          if (mapped eq null) unchanged
          else mapped.prependToList(unchanged)
        } else {
          val head0 = pending.head
          val head1 = f(head0)

          if (head1.asInstanceOf[AnyRef] eq head0.asInstanceOf[AnyRef])
            loop(mapped, unchanged, pending.tail)
          else {
            val b = if (mapped eq null) new ListBuffer[U] else mapped
            var xc = unchanged
            while (xc ne pending) {
              b += xc.head
              xc = xc.tail
            }
            b += head1
            val tail0 = pending.tail
            loop(b, tail0.asInstanceOf[List[U]], tail0)
          }
        }
      loop(null, xs.asInstanceOf[List[U]], xs)
    }

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
      if (xs.isEmpty || ys.isEmpty) Nil
      else {
        val x1 = f(xs.head, ys.head)
        val xs1 = xs.tail.zipWithConserve(ys.tail)(f)
        if ((x1.asInstanceOf[AnyRef] eq xs.head.asInstanceOf[AnyRef]) &&
            (xs1 eq xs.tail)) xs
        else x1 :: xs1
      }

    final def hasSameLengthAs[U](ys: List[U]): Boolean = {
      @tailrec def loop(xs: List[T], ys: List[U]): Boolean =
        if (xs.isEmpty) ys.isEmpty
        else ys.nonEmpty && loop(xs.tail, ys.tail)
      loop(xs, ys)
    }

    @tailrec final def eqElements(ys: List[AnyRef]): Boolean = xs match {
      case x :: _ =>
        ys match {
          case y :: _ =>
            x.asInstanceOf[AnyRef].eq(y) &&
            xs.tail.eqElements(ys.tail)
          case _ => false
        }
      case nil => ys.isEmpty
    }

    /** Union on lists seen as sets */
    def | (ys: List[T]): List[T] = xs ::: (ys filterNot (xs contains _))

    /** Intersection on lists seen as sets */
    def & (ys: List[T]): List[T] = xs filter (ys contains _)
  }

  implicit class ListOfListDecorator[T](val xss: List[List[T]]) extends AnyVal {
    def nestedMap[U](f: T => U): List[List[U]] = xss map (_ map f)
    def nestedMapconserve[U](f: T => U): List[List[U]] = xss mapconserve (_ mapconserve f)
  }

  implicit class TextToString(val text: Text) extends AnyVal {
    def show(implicit ctx: Context): String = text.mkString(ctx.settings.pageWidth.value, ctx.settings.printLines.value)
  }

  /** Test whether a list of strings representing phases contains
   *  a given phase. See [[config.CompilerCommand#explainAdvanced]] for the
   *  exact meaning of "contains" here.
   */
  implicit class PhaseListDecorator(val names: List[String]) extends AnyVal {
    def containsPhase(phase: Phase): Boolean =
      names.nonEmpty && {
        phase match {
          case phase: MegaPhase => phase.miniPhases.exists(containsPhase)
          case _ =>
            names exists { name =>
              name == "all" || {
                val strippedName = name.stripSuffix("+")
                val logNextPhase = name != strippedName
                phase.phaseName.startsWith(strippedName) ||
                  (logNextPhase && phase.prev.phaseName.startsWith(strippedName))
              }
            }
        }
      }
  }

  implicit class genericDeco[T](val x: T) extends AnyVal {
    def reporting(op: T => String, printer: config.Printers.Printer = config.Printers.default): T = {
      printer.println(op(x))
      x
    }
    def assertingErrorsReported(implicit ctx: Context): T = {
      assert(ctx.reporter.errorsReported)
      x
    }
    def assertingErrorsReported(msg: => String)(implicit ctx: Context): T = {
      assert(ctx.reporter.errorsReported, msg)
      x
    }
  }

  implicit class StringInterpolators(val sc: StringContext) extends AnyVal {
    /** General purpose string formatting */
    def i(args: Any*)(implicit ctx: Context): String =
      new StringFormatter(sc).assemble(args)

    /** Formatting for error messages: Like `i` but suppress follow-on
     *  error messages after the first one if some of their arguments are "non-sensical".
     */
    def em(args: Any*)(implicit ctx: Context): String =
      new ErrorMessageFormatter(sc).assemble(args)

    /** Formatting with added explanations: Like `em`, but add explanations to
     *  give more info about type variables and to disambiguate where needed.
     */
    def ex(args: Any*)(implicit ctx: Context): String =
      explained(implicit ctx => em(args: _*))
  }

  implicit class ArrayInterpolator[T <: AnyRef](val arr: Array[T]) extends AnyVal {
    def binarySearch(x: T): Int = java.util.Arrays.binarySearch(arr.asInstanceOf[Array[Object]], x)
  }
}

