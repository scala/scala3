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

/** This object provides useful conversions and extension methods for types defined elsewhere */
object DecoratorsX {

  /** Turns Strings into PreNames, adding toType/TermName methods */
  class StringPreName(s: String) extends AnyVal with PreName {
    def toTypeName: TypeName = typeName(s)
    def toTermName: TermName = termName(s)
    def toText(printer: Printer): Text = Str(s)
  }
  delegate for Conversion[String, StringPreName] = new StringPreName(_)

  final val MaxFilterRecursions = 1000

  delegate {
    def (s: String) splitWhere (f: Char => Boolean, doDropIndex: Boolean): Option[(String, String)] = {
      def splitAt(idx: Int, doDropIndex: Boolean): Option[(String, String)] =
        if (idx == -1) None
        else Some((s.take(idx), s.drop(if (doDropIndex) idx + 1 else idx)))

      splitAt(s.indexWhere(f), doDropIndex)
    }

    /** Implements a findSymbol method on iterators of Symbols that
     *  works like find but avoids Option, replacing None with NoSymbol.
     */
    def (it: Iterator[Symbol]) findSymbol(p: Symbol => Boolean): Symbol = {
      while (it.hasNext) {
        val sym = it.next()
        if (p(sym)) return sym
      }
      NoSymbol
    }

    def (xs: List[T]) mapconserve [T, U] (f: T => U): List[U] = {
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
    def (xs: List[T]) filterConserve [T] (p: T => Boolean): List[T] = {
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
    def (xs: List[T]) zipWithConserve [T, U] (ys: List[U])(f: (T, U) => T): List[T] =
      if (xs.isEmpty || ys.isEmpty) Nil
      else {
        val x1 = f(xs.head, ys.head)
        val xs1 = xs.tail.zipWithConserve(ys.tail)(f)
        if ((x1.asInstanceOf[AnyRef] eq xs.head.asInstanceOf[AnyRef]) &&
            (xs1 eq xs.tail)) xs
        else x1 :: xs1
      }

    def (xs: List[T]) hasSameLengthAs [T, U] (ys: List[U]): Boolean = {
      @tailrec def loop(xs: List[T], ys: List[U]): Boolean =
        if (xs.isEmpty) ys.isEmpty
        else ys.nonEmpty && loop(xs.tail, ys.tail)
      loop(xs, ys)
    }

    @tailrec
    def (xs: List[T]) eqElements [T] (ys: List[AnyRef]): Boolean = xs match {
      case x :: _ =>
        ys match {
          case y :: _ =>
            x.asInstanceOf[AnyRef].eq(y) &&
            xs.tail.eqElements(ys.tail)
          case _ => false
        }
      case nil => ys.isEmpty
    }

    def (xs: List[T]) | [T] (ys: List[T]): List[T] = xs ::: (ys filterNot (xs contains _))

    /** Intersection on lists seen as sets */
    def (xs: List[T]) & [T] (ys: List[T]): List[T] = xs filter (ys contains _)

    def (xss: List[List[T]]) nestedMap [T, U] (f: T => U): List[List[U]] = xss map (_ map f)
    def (xss: List[List[T]]) nestedMapconserve [T, U](f: T => U): List[List[U]] = xss mapconserve (_ mapconserve f)

    def (text: Text) show given (ctx: Context): String =
      text.mkString(ctx.settings.pageWidth.value, ctx.settings.printLines.value)

    /** Test whether a list of strings representing phases contains
    *  a given phase. See [[config.CompilerCommand#explainAdvanced]] for the
    *  exact meaning of "contains" here.
    */
    def (names: List[String]) containsPhase (phase: Phase): Boolean =
      names.nonEmpty && {
        phase match {
          case phase: MegaPhase => phase.miniPhases.exists(names.containsPhase(_))
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

    def (x: T) reporting [T] (op: T => String, printer: config.Printers.Printer = config.Printers.default): T = {
      printer.println(op(x))
      x
    }

    def (x: T) assertingErrorsReported [T] given Context: T = {
      assert(the[Context].reporter.errorsReported)
      x
    }

    def (x: T) assertingErrorsReported [T] (msg: => String) given Context: T = {
      assert(the[Context].reporter.errorsReported, msg)
      x
    }

    /** General purpose string formatting */
    def (sc: StringContext) i (args: Any*) given Context: String =
      new StringFormatter(sc).assemble(args)

    /** Formatting for error messages: Like `i` but suppress follow-on
     *  error messages after the first one if some of their arguments are "non-sensical".
     */
    def (sc: StringContext) em (args: Any*) given Context: String =
      new ErrorMessageFormatter(sc).assemble(args)

    /** Formatting with added explanations: Like `em`, but add explanations to
     *  give more info about type variables and to disambiguate where needed.
     */
    def (sc: StringContext) ex (args: Any*) given Context: String =
      explained(sc.em(args: _*))

    /** Formatter that adds syntax highlighting to all interpolated values */
    def (sc: StringContext) hl (args: Any*) given Context: String =
      new SyntaxFormatter(sc).assemble(args).stripMargin

    def (arr: Array[T]) binarySearch [T <: AnyRef] (x: T): Int =
      java.util.Arrays.binarySearch(arr.asInstanceOf[Array[Object]], x)
  }

    /** Entrypoint for explanation string interpolator:
    *
    * ```
    * ex"disambiguate $tpe1 and $tpe2"
    * ```
    */
  def explained(op: given Context => String) given Context: String =
    printing.Formatting.explained(ctx => op given ctx)
}
