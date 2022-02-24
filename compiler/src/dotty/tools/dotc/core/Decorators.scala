package dotty.tools
package dotc
package core

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

import Contexts._, Names._, Phases._, Symbols._
import printing.{ Printer, Showable }, printing.Formatting._, printing.Texts._
import transform.MegaPhase

/** This object provides useful implicit decorators for types defined elsewhere */
object Decorators {

  /** Extension methods for toType/TermName methods on strings.
   *  They are in an implicit object for now, so that we can import decorators
   *  with a normal wildcard. In the future, once #9255 is in trunk, replace with
   *  a simple collective extension.
   */
  extension (pn: PreName)
    def toTermName: TermName = pn match
      case s: String => termName(s)
      case n: Name => n.toTermName
    def toTypeName: TypeName = pn match
      case s: String => typeName(s)
      case n: Name => n.toTypeName

  extension (s: String)
    def splitWhere(f: Char => Boolean, doDropIndex: Boolean): Option[(String, String)] =
      def splitAt(idx: Int, doDropIndex: Boolean): Option[(String, String)] =
        if (idx == -1) None
        else Some((s.take(idx), s.drop(if (doDropIndex) idx + 1 else idx)))
      splitAt(s.indexWhere(f), doDropIndex)

    /** Create a term name from a string slice, using a common buffer.
     *  This avoids some allocation relative to `termName(s)`
     */
    def sliceToTermName(start: Int, end: Int)(using Context): SimpleName =
      val len = end - start
      val chars = ctx.base.sharedCharArray(len)
      s.getChars(start, end, chars, 0)
      termName(chars, 0, len)

    def sliceToTypeName(start: Int, end: Int)(using Context): TypeName =
      sliceToTermName(start, end).toTypeName

    def concat(name: Name)(using Context): SimpleName = name match
      case name: SimpleName =>
        val len = s.length + name.length
        var chars = ctx.base.sharedCharArray(len)
        s.getChars(0, s.length, chars, 0)
        if name.length != 0 then name.getChars(0, name.length, chars, s.length)
        termName(chars, 0, len)
      case name: TypeName => s.concat(name.toTermName)
      case _ => termName(s.concat(name.toString).nn)

    def indented(width: Int): String =
      val padding = " " * width
      padding + s.replace("\n", "\n" + padding)
  end extension

  /** Implements a findSymbol method on iterators of Symbols that
   *  works like find but avoids Option, replacing None with NoSymbol.
   */
  extension (it: Iterator[Symbol])
    final def findSymbol(p: Symbol => Boolean): Symbol = {
      while (it.hasNext) {
        val sym = it.next()
        if (p(sym)) return sym
      }
      NoSymbol
    }

  inline val MaxFilterRecursions = 10

  /** Implements filterConserve, zipWithConserve methods
   *  on lists that avoid duplication of list nodes where feasible.
   */
  implicit class ListDecorator[T](val xs: List[T]) extends AnyVal {

    final def mapconserve[U](f: T => U): List[U] = {
      @tailrec
      def loop(mapped: ListBuffer[U] | Null, unchanged: List[U], pending: List[T]): List[U] =
        if (pending.isEmpty)
          if (mapped == null) unchanged
          else mapped.prependToList(unchanged)
        else {
          val head0 = pending.head
          val head1 = f(head0)

          if (head1.asInstanceOf[AnyRef] eq head0.asInstanceOf[AnyRef])
            loop(mapped, unchanged, pending.tail)
          else {
            val b = if (mapped == null) new ListBuffer[U] else mapped
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
     *  if `p` is true for all elements.
     */
    def filterConserve(p: T => Boolean): List[T] =

      def addAll(buf: ListBuffer[T], from: List[T], until: List[T]): ListBuffer[T] =
        if from eq until then buf else addAll(buf += from.head, from.tail, until)

      def loopWithBuffer(buf: ListBuffer[T], xs: List[T]): List[T] = xs match
        case x :: xs1 =>
          if p(x) then buf += x
          loopWithBuffer(buf, xs1)
        case nil => buf.toList

      def loop(keep: List[T], explore: List[T], keepCount: Int, recCount: Int): List[T] =
        explore match
          case x :: rest =>
            if p(x) then
              loop(keep, rest, keepCount + 1, recCount)
            else if keepCount <= 3 && recCount <= MaxFilterRecursions then
              val rest1 = loop(rest, rest, 0, recCount + 1)
              keepCount match
                case 0 => rest1
                case 1 => keep.head :: rest1
                case 2 => keep.head :: keep.tail.head :: rest1
                case 3 => val tl = keep.tail; keep.head :: tl.head :: tl.tail.head :: rest1
            else
              loopWithBuffer(addAll(new ListBuffer[T], keep, explore), rest)
          case nil =>
            keep

      loop(xs, xs, 0, 0)
    end filterConserve

    /** Like `xs.lazyZip(ys).map(f)`, but returns list `xs` itself
     *  - instead of a copy - if function `f` maps all elements of
     *  `xs` to themselves. Also, it is required that `ys` is at least
     *  as long as `xs`.
     */
    def zipWithConserve[U, V <: T](ys: List[U])(f: (T, U) => V): List[V] =
      if (xs.isEmpty || ys.isEmpty) Nil
      else {
        val x1 = f(xs.head, ys.head)
        val xs1 = xs.tail.zipWithConserve(ys.tail)(f)
        if (x1.asInstanceOf[AnyRef] eq xs.head.asInstanceOf[AnyRef]) && (xs1 eq xs.tail)
          then xs.asInstanceOf[List[V]]
        else x1 :: xs1
      }

    /** Like `xs.lazyZip(xs.indices).map(f)`, but returns list `xs` itself
     *  - instead of a copy - if function `f` maps all elements of
     *  `xs` to themselves.
     */
    def mapWithIndexConserve[U <: T](f: (T, Int) => U): List[U] =

      @tailrec
      def addAll(buf: ListBuffer[T], from: List[T], until: List[T]): ListBuffer[T] =
        if from eq until then buf else addAll(buf += from.head, from.tail, until)

      @tailrec
      def loopWithBuffer(buf: ListBuffer[U], explore: List[T], idx: Int): List[U] = explore match
        case Nil       => buf.toList
        case t :: rest => loopWithBuffer(buf += f(t, idx), rest, idx + 1)

      @tailrec
      def loop(keep: List[T], explore: List[T], idx: Int): List[U] = explore match
        case Nil => keep.asInstanceOf[List[U]]
        case t :: rest =>
          val u = f(t, idx)
          if u.asInstanceOf[AnyRef] eq t.asInstanceOf[AnyRef] then
            loop(keep, rest, idx + 1)
          else
            val buf = addAll(new ListBuffer[T], keep, explore).asInstanceOf[ListBuffer[U]]
            loopWithBuffer(buf += u, rest, idx + 1)

      loop(xs, xs, 0)
    end mapWithIndexConserve

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

  extension [T, U](xss: List[List[T]])
    def nestedMap(f: T => U): List[List[U]] = xss match
      case xs :: xss1 => xs.map(f) :: xss1.nestedMap(f)
      case nil => Nil
    def nestedMapConserve(f: T => U): List[List[U]] =
      xss.mapconserve(_.mapconserve(f))
    def nestedZipWithConserve(yss: List[List[U]])(f: (T, U) => T): List[List[T]] =
      xss.zipWithConserve(yss)((xs, ys) => xs.zipWithConserve(ys)(f))
    def nestedExists(p: T => Boolean): Boolean = xss match
      case xs :: xss1 => xs.exists(p) || xss1.nestedExists(p)
      case nil => false
  end extension

  extension (text: Text)
    def show(using Context): String = text.mkString(ctx.settings.pageWidth.value, ctx.settings.printLines.value)

  /** Test whether a list of strings representing phases contains
   *  a given phase. See [[config.CompilerCommand#explainAdvanced]] for the
   *  exact meaning of "contains" here.
   */
   extension (names: List[String])
    def containsPhase(phase: Phase): Boolean =
      names.nonEmpty && {
        phase match {
          case phase: MegaPhase => phase.miniPhases.exists(x => names.containsPhase(x))
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

  extension [T](x: T)
    def showing[U](
        op: WrappedResult[U] ?=> String,
        printer: config.Printers.Printer = config.Printers.default)(using c: Conversion[T, U] = null): T = {
      // either the use of `$result` was driven by the expected type of `Shown`
      // which led to the summoning of `Conversion[T, Shown]` (which we'll invoke)
      // or no such conversion was found so we'll consume the result as it is instead
      val obj = if c == null then x.asInstanceOf[U] else c(x)
      printer.println(op(using WrappedResult(obj)))
      x
    }

    /** Instead of `toString` call `show` on `Showable` values, falling back to `toString` if an exception is raised. */
    def show(using Context): String = x match
      case x: Showable =>
        try x.show
        catch
          case ex: CyclicReference => "... (caught cyclic reference) ..."
          case NonFatal(ex)
              if !ctx.mode.is(Mode.PrintShowExceptions) && !ctx.settings.YshowPrintErrors.value =>
            val msg = ex match { case te: TypeError => te.toMessage case _ => ex.getMessage }
            s"[cannot display due to $msg, raw string = $x]"
      case _ => String.valueOf(x)

  extension [T](x: T)
    def assertingErrorsReported(using Context): T = {
      assert(ctx.reporter.errorsReported)
      x
    }
    def assertingErrorsReported(msg: => String)(using Context): T = {
      assert(ctx.reporter.errorsReported, msg)
      x
    }

  extension [T <: AnyRef](xs: ::[T])
    def derivedCons(x1: T, xs1: List[T]) =
      if (xs.head eq x1) && (xs.tail eq xs1) then xs else x1 :: xs1

  extension (sc: StringContext)
    /** General purpose string formatting */
    def i(args: Shown*)(using Context): String =
      new StringFormatter(sc).assemble(args)

    /** Formatting for error messages: Like `i` but suppress follow-on
     *  error messages after the first one if some of their arguments are "non-sensical".
     */
    def em(args: Shown*)(using Context): String =
      new ErrorMessageFormatter(sc).assemble(args)

    /** Formatting with added explanations: Like `em`, but add explanations to
     *  give more info about type variables and to disambiguate where needed.
     */
    def ex(args: Shown*)(using Context): String =
      explained(em(args: _*))

  extension [T <: AnyRef](arr: Array[T])
    def binarySearch(x: T | Null): Int = java.util.Arrays.binarySearch(arr.asInstanceOf[Array[Object | Null]], x)

}
