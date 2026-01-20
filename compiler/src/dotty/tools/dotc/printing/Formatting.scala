package dotty.tools
package dotc
package printing

import core.*
import Texts.*, Types.*, Flags.*, Symbols.*, Contexts.*
import Decorators.*
import reporting.Message
import util.{DiffUtil, SimpleIdentitySet}
import Highlighting.*

object Formatting {

  /** Essentially, a function Context => T, which can be created with `delay` */
  abstract class Delay[T]:
    def apply(c: Context): T

  /** Delay a Context => T computation so that it is generated from the embedded
   *  context of a string formatter instead of the enclosing context. This is needed
   *  to make disambiguation work for such embedded computations.
   */
  def delay[T](fn: Context ?=> T): Delay[T] = new Delay[T]:
    def apply(c: Context) = fn(using c)

  object ShownDef:
    /** Represents a value that has been "shown" and can be consumed by StringFormatter.
     *  Not just a string because it may be a Seq that StringFormatter will intersperse with the trailing separator.
     *  It may also be a CtxShow, which allows the Show instance to finish showing the value with the string
     *  interpolator's correct context, that is with non-sensical tagging, message limiting, explanations, etc. */
    opaque type Shown = Any
    object Shown:
      given [A: Show]: Conversion[A, Shown] = Show[A].show(_)

      extension (s: Shown)
        def runCtxShow(using Context): Shown = s match
          case cs: CtxShow => cs.run
          case _           => s

      def toStr(x: Shown)(using Context): String = x match
        case seq: Seq[?] => seq.map(toStr).mkString("[", ", ", "]")
        case res         => res.tryToShow

    import Shown.runCtxShow

    sealed abstract class Show[-T]:
      /** Show a value T by returning a "shown" result. */
      def show(x: T): Shown

    trait CtxShow:
      def run(using Context): Shown

    private inline def CtxShow(inline x: Context ?=> Shown) =
      class InlinedCtxShow extends CtxShow { def run(using Context) = x(using ctx) }
      new InlinedCtxShow
    private def toStr[A: Show](x: A)(using Context): String = Shown.toStr(toShown(x))
    private def toShown[A: Show](x: A)(using Context): Shown = Show[A].show(x).runCtxShow

    /** The base implementation, passing the argument to StringFormatter which will try to `.show` it. */
    object ShowAny extends Show[Any]:
      def show(x: Any): Shown = x

    class ShowImplicits4:
      given [X: Show]: Show[X | Null] with
        def show(x: X | Null) = if x == null then "null" else CtxShow(toStr(x))

    class ShowImplicits3 extends ShowImplicits4:
      given Show[Product] = ShowAny

    class ShowImplicits2 extends ShowImplicits3:
      given Show[ParamInfo] = ShowAny

    class ShowImplicits1 extends ShowImplicits2:
      given Show[ImplicitRef]      = ShowAny
      given Show[Names.Designator] = ShowAny
      given Show[util.SrcPos]      = ShowAny

    object Show extends ShowImplicits1:
      inline def apply[A](using inline z: Show[A]): Show[A] = z

      given [X: Show]: Show[Option[X]] with
        def show(x: Option[X]) =
          CtxShow(x.map(toStr))
      end given

      given [X: Show]: Show[Seq[X]] with
        def show(x: Seq[X]) = CtxShow(x.map(toStr))

      given [X: Show]: Show[Delay[X]] = new Show:
        def show(x: Delay[X]) = CtxShow(c ?=> x(c))

      given Show[Seq[Nothing]] with
        def show(x: Seq[Nothing]) = CtxShow(x)

      given [K: Show, V: Show]: Show[Map[K, V]] with
        def show(x: Map[K, V]) =
          CtxShow(x.map((k, v) => s"${toStr(k)} => ${toStr(v)}"))

      given [H: Show, T <: Tuple: Show]: Show[H *: T] with
        def show(x: H *: T) =
          CtxShow(toStr(x.head) *: toShown(x.tail).asInstanceOf[Tuple])

      given [X <: AnyRef: Show]: Show[SimpleIdentitySet[X]] with
        def show(x: SimpleIdentitySet[X]) = summon[Show[List[X]]].show(x.toList)

      given Show[FlagSet] with
        def show(x: FlagSet) = x.flagsString

      given Show[TypeComparer.ApproxState] with
        def show(x: TypeComparer.ApproxState) = TypeComparer.ApproxState.Repr.show(x)

      given Show[ast.TreeInfo.PurityLevel] with
        def show(x: ast.TreeInfo.PurityLevel) = x match
          case ast.TreeInfo.Path           => "PurityLevel.Path"
          case ast.TreeInfo.Pure           => "PurityLevel.Pure"
          case ast.TreeInfo.Idempotent     => "PurityLevel.Idempotent"
          case ast.TreeInfo.Impure         => "PurityLevel.Impure"
          case ast.TreeInfo.PurePath       => "PurityLevel.PurePath"
          case ast.TreeInfo.IdempotentPath => "PurityLevel.IdempotentPath"
          case _                           => s"PurityLevel(${x.x.toBinaryString})"

      given Show[Atoms] with
        def show(x: Atoms) = x match
          case Atoms.Unknown       => "Unknown"
          case Atoms.Range(lo, hi) => CtxShow(s"Range(${toStr(lo.toList)}, ${toStr(hi.toList)})")
      end given

      given Show[ast.untpd.Modifiers] with
        def show(x: ast.untpd.Modifiers) =
          CtxShow(s"Modifiers(${toStr(x.flags)}, ${toStr(x.privateWithin)}, ${toStr(x.annotations)}, ${toStr(x.mods)})")

      given Show[ast.untpd.Mod] with
        def show(x: ast.untpd.Mod) = CtxShow(s"Mod(${toStr(x.flags)})")

      given Show[Showable]                            = ShowAny
      given Show[Shown]                               = ShowAny
      given Show[Int]                                 = ShowAny
      given Show[Char]                                = ShowAny
      given Show[Boolean]                             = ShowAny
      given Show[Integer]                             = ShowAny
      given Show[Long]                                = ShowAny
      given Show[String]                              = ShowAny
      given Show[Class[?]]                            = ShowAny
      given Show[Throwable]                           = ShowAny
      given Show[StringBuffer]                        = ShowAny
      given Show[StringBuilder]                       = ShowAny
      given Show[CompilationUnit]                     = ShowAny
      given Show[Phases.Phase]                        = ShowAny
      given Show[TyperState]                          = ShowAny
      given Show[Unit]                                = ShowAny
      given Show[config.ScalaVersion]                 = ShowAny
      given Show[io.AbstractFile]                     = ShowAny
      given Show[parsing.Scanners.Scanner]            = ShowAny
      given Show[util.SourceFile]                     = ShowAny
      given Show[util.Spans.Span]                     = ShowAny
      given Show[tasty.TreeUnpickler#OwnerTree]       = ShowAny
      given Show[typer.ForceDegree.Value]             = ShowAny
    end Show
  end ShownDef
  export ShownDef.{ Show, Shown }

  /** General purpose string formatter, with the following features:
   *
   *  1. Invokes the `show` extension method on the interpolated arguments.
   *  2. Sequences can be formatted using the desired separator between two `%` signs,
   *     eg `i"myList = (${myList}%, %)"`
   *  3. Safe handling of multi-line margins. Left margins are stripped on the parts
   *     of the string context *before* inserting the arguments. That way, we guard
   *     against accidentally treating an interpolated value as a margin.
   */
  class StringFormatter(protected val sc: StringContext) {
    protected def showArg(arg: Any)(using Context): String = arg.tryToShow

    private def treatArg(arg: Shown, suffix: String)(using Context): (String, String) = arg.runCtxShow match {
      case arg: Seq[?] if suffix.indexOf('%') == 0 && suffix.indexOf('%', 1) != -1 =>
        val end = suffix.indexOf('%', 1)
        val sep = StringContext.processEscapes(suffix.substring(1, end).nn)
        (arg.mkString(sep), suffix.substring(end + 1).nn)
      case arg: Seq[?] =>
        (arg.map(showArg).mkString("[", ", ", "]"), suffix)
      case arg =>
        (showArg(arg), suffix)
    }

    def assemble(args: Seq[Shown])(using Context): String = {
      def isLineBreak(c: Char) = c == '\n' || c == '\f' // compatible with StringLike#isLineBreak
      def stripTrailingPart(s: String) = {
        val (pre, post) = s.span(c => !isLineBreak(c))
        pre ++ post.stripMargin
      }
      val (prefix, suffixes) = sc.parts.toList match {
        case head :: tail => (head.stripMargin, tail map stripTrailingPart)
        case Nil => ("", Nil)
      }
      val (args1, suffixes1) = args.lazyZip(suffixes).map(treatArg(_, _)).unzip
      new StringContext(prefix :: suffixes1.toList*).s(args1*)
    }
  }

  /** This method will produce a colored type diff from the given arguments.
    * The idea is to do this for known cases that are useful and then fall back
    * on regular syntax highlighting for the cases which are unhandled.
    *
    * Please note that if used in combination with `disambiguateTypes` the
    * correct `Context` for printing should also be passed when calling the
    * method.
    *
    * @return the (found, expected, changePercentage) with coloring to
    *         highlight the difference
    */
  def typeDiff(found: Type, expected: Type)(using Context): (String, String) =
    val fnd = found.show
    val exp = expected.show
    DiffUtil.mkColoredTypeDiff(fnd, exp) match
      case (fnd1, exp1, change)
      if change < 0.5 && ctx.settings.color.value != "never" => (fnd1, exp1)
      case _ => (fnd, exp)

  /** Explicit syntax highlighting */
  def hl(s: String)(using Context): String =
    SyntaxHighlighting.highlight(s)

  /** Explicitly highlight a string with the same formatting as used for keywords */
  def hlAsKeyword(str: String)(using Context): String =
    if str.isEmpty || ctx.settings.color.value == "never" then str
    else s"${SyntaxHighlighting.KeywordColor}$str${SyntaxHighlighting.NoColor}"
}
