package dotty.tools.dotc
package core

import annotation.tailrec
import Symbols._
import Contexts._, Names._, Phases._, printing.Texts._, printing.Printer, printing.Showable
import util.Positions.Position, util.SourcePosition
import collection.mutable.ListBuffer
import dotty.tools.dotc.transform.TreeTransforms._
import typer.Mode
import scala.language.implicitConversions

/** This object provides useful implicit decorators for types defined elsewhere */
object Decorators {

  /** Turns Strings into PreNames, adding toType/TermName methods */
  implicit class StringDecorator(val s: String) extends AnyVal with PreName {
    def toTypeName: TypeName = typeName(s)
    def toTermName: TermName = termName(s)
    def toText(printer: Printer): Text = Str(s)
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

  /** Implements filterConserve, zipWithConserve methods
   *  on lists that avoid duplication of list nodes where feasible.
   */
  implicit class ListDecorator[T](val xs: List[T]) extends AnyVal {

    @inline final def mapconserve[U](f: T => U): List[U] = {
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
      if (xs.isEmpty) xs
      else {
        val x1 = f(xs.head, ys.head)
        val xs1 = xs.tail.zipWithConserve(ys.tail)(f)
        if ((x1.asInstanceOf[AnyRef] eq xs.head.asInstanceOf[AnyRef]) &&
            (xs1 eq xs.tail)) xs
        else x1 :: xs1
      }

    def foldRightBN[U](z: => U)(op: (T, => U) => U): U = xs match {
      case Nil => z
      case x :: xs1 => op(x, xs1.foldRightBN(z)(op))
    }

    final def hasSameLengthAs[U](ys: List[U]): Boolean = {
      @tailrec def loop(xs: List[T], ys: List[U]): Boolean =
        if (xs.isEmpty) ys.isEmpty
        else ys.nonEmpty && loop(xs.tail, ys.tail)
      loop(xs, ys)
    }

    /** Union on lists seen as sets */
    def | (ys: List[T]): List[T] = xs ++ (ys filterNot (xs contains _))

    /** Intersection on lists seen as sets */
    def & (ys: List[T]): List[T] = xs filter (ys contains _)
  }

  implicit class ListOfListDecorator[T](val xss: List[List[T]]) extends AnyVal {
    def nestedMap[U](f: T => U): List[List[U]] = xss map (_ map f)
    def nestedMapconserve[U](f: T => U): List[List[U]] = xss mapconserve (_ mapconserve f)
  }

  implicit class TextToString(val text: Text) extends AnyVal {
    def show(implicit ctx: Context) = text.mkString(ctx.settings.pageWidth.value)
  }

  /** Test whether a list of strings representing phases contains
   *  a given phase. See [[config.CompilerCommand#explainAdvanced]] for the
   *  exact meaning of "contains" here.
   */
  implicit class PhaseListDecorator(val names: List[String]) extends AnyVal {
    def containsPhase(phase: Phase): Boolean = phase match {
      case phase: TreeTransformer => phase.transformations.exists(trans => containsPhase(trans.phase))
      case _ =>
        names exists { name =>
          name == "all" || {
            val strippedName = name.stripSuffix("+")
            val logNextPhase = name ne strippedName
            phase.phaseName.startsWith(strippedName) ||
              (logNextPhase && phase.prev.phaseName.startsWith(strippedName))
          }
        }
    }
  }

  implicit def sourcePos(pos: Position)(implicit ctx: Context): SourcePosition =
    ctx.source.atPos(pos)

  /** The i"..." string interpolator adds two features to the s interpolator:
   *  1) On all Showables, `show` is called instead of `toString`
   *  2) Lists can be formatted using the desired separator between two `%` signs,
   *     eg `i"myList = (${myList}%, %)"`
   */
  implicit class StringInterpolators(val sc: StringContext) extends AnyVal {

    def i(args: Any*)(implicit ctx: Context): String = {

      def treatArg(arg: Any, suffix: String): (Any, String) = arg match {
        case arg: Seq[_] if suffix.nonEmpty && suffix.head == '%' =>
          val (rawsep, rest) = suffix.tail.span(_ != '%')
          val sep = StringContext.treatEscapes(rawsep)
          if (rest.nonEmpty) (arg map treatSingleArg mkString sep, rest.tail)
          else (arg, suffix)
        case _ =>
          (treatSingleArg(arg), suffix)
      }

      def treatSingleArg(arg: Any) : Any =
        try
          arg match {
            case arg: Showable => arg.show(ctx.addMode(Mode.FutureDefsOK))
            case _ => arg
          }
        catch {
          case ex: Exception => throw ex // s"(missing due to $ex)"
        }

      val prefix :: suffixes = sc.parts.toList
      val (args1, suffixes1) = (args, suffixes).zipped.map(treatArg(_, _)).unzip
      new StringContext(prefix :: suffixes1.toList: _*).s(args1: _*)
    }

    /** Lifted from scala.reflect.internal.util
     *  A safe combination of [[scala.collection.immutable.StringLike#stripMargin]]
     *  and [[scala.StringContext#raw]].
     *
     *  The margin of each line is defined by whitespace leading up to a '|' character.
     *  This margin is stripped '''before''' the arguments are interpolated into to string.
     *
     *  String escape sequences are '''not''' processed; this interpolater is designed to
     *  be used with triple quoted Strings.
     *
     *  {{{
     *  scala> val foo = "f|o|o"
     *  foo: String = f|o|o
     *  scala> sm"""|${foo}
     *             |"""
     *  res0: String =
     *  "f|o|o
     *  "
     *  }}}
     */
    final def sm(args: Any*): String = {
      def isLineBreak(c: Char) = c == '\n' || c == '\f' // compatible with StringLike#isLineBreak
      def stripTrailingPart(s: String) = {
        val (pre, post) = s.span(c => !isLineBreak(c))
        pre + post.stripMargin
      }
      val stripped: List[String] = sc.parts.toList match {
        case head :: tail => head.stripMargin :: (tail map stripTrailingPart)
        case Nil => Nil
      }
      new StringContext(stripped: _*).raw(args: _*)
    }
  }
}

