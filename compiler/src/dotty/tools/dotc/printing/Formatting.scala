package dotty.tools
package dotc
package printing

import scala.language.unsafeNulls

import scala.collection.mutable

import core._
import Texts._, Types._, Flags._, Symbols._, Contexts._
import Decorators._
import reporting.Message
import util.DiffUtil
import Highlighting._

object Formatting {

  object ShownDef:
    /** Represents a value that has been "shown" and can be consumed by StringFormatter.
     *  Not just a string because it may be a Seq that StringFormatter will intersperse with the trailing separator.
     *  Also, it's not a `String | Seq[String]` because then we'd need a Context to call `Showable#show`.  We could
     *  make Context a requirement for a Show instance but then we'd have lots of instances instead of just one ShowAny
     *  instance.  We could also try to make `Show#show` require the Context, but then that breaks the Conversion. */
    opaque type Shown = Any
    object Shown:
      given [A: Show]: Conversion[A, Shown] = Show[A].show(_)

    sealed abstract class Show[-T]:
      /** Show a value T by returning a "shown" result. */
      def show(x: T): Shown

    /** The base implementation, passing the argument to StringFormatter which will try to `.show` it. */
    object ShowAny extends Show[Any]:
      def show(x: Any): Shown = x

    class ShowImplicits3:
      given Show[Product] = ShowAny

    class ShowImplicits2 extends ShowImplicits3:
      given Show[ParamInfo] with
        def show(x: ParamInfo) = x match
          case x: Symbol      => Show[x.type].show(x)
          case x: LambdaParam => Show[x.type].show(x)
          case _              => ShowAny

    class ShowImplicits1 extends ShowImplicits2:
      given Show[ImplicitRef]      = ShowAny
      given Show[Names.Designator] = ShowAny
      given Show[util.SrcPos]      = ShowAny

    object Show extends ShowImplicits1:
      inline def apply[A](using inline z: Show[A]): Show[A] = z

      given [X: Show]: Show[Seq[X]] with
        def show(x: Seq[X]) = x.map(Show[X].show)

      given [A: Show, B: Show]: Show[(A, B)] with
        def show(x: (A, B)) = (Show[A].show(x._1), Show[B].show(x._2))

      given [X: Show]: Show[X | Null] with
        def show(x: X | Null) = if x == null then "null" else Show[X].show(x.nn)

      given Show[FlagSet] with
        def show(x: FlagSet) = x.flagsString

      given Show[TypeComparer.ApproxState] with
        def show(x: TypeComparer.ApproxState) = TypeComparer.ApproxState.Repr.show(x)

      given Show[Showable]                            = ShowAny
      given Show[Shown]                               = ShowAny
      given Show[Int]                                 = ShowAny
      given Show[Char]                                = ShowAny
      given Show[Boolean]                             = ShowAny
      given Show[String]                              = ShowAny
      given Show[Class[?]]                            = ShowAny
      given Show[Throwable]                           = ShowAny
      given Show[StringBuffer]                        = ShowAny
      given Show[CompilationUnit]                     = ShowAny
      given Show[Phases.Phase]                        = ShowAny
      given Show[TyperState]                          = ShowAny
      given Show[config.ScalaVersion]                 = ShowAny
      given Show[io.AbstractFile]                     = ShowAny
      given Show[parsing.Scanners.Scanner]            = ShowAny
      given Show[util.SourceFile]                     = ShowAny
      given Show[util.Spans.Span]                     = ShowAny
      given Show[tasty.TreeUnpickler#OwnerTree]       = ShowAny
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

    private def treatArg(arg: Shown, suffix: String)(using Context): (Any, String) = arg match {
      case arg: Seq[?] if suffix.nonEmpty && suffix.head == '%' =>
        val (rawsep, rest) = suffix.tail.span(_ != '%')
        val sep = StringContext.processEscapes(rawsep)
        if (rest.nonEmpty) (arg.map(showArg).mkString(sep), rest.tail)
        else (arg, suffix)
      case arg: Seq[?] =>
        (arg.map(showArg).mkString("[", ", ", "]"), suffix)
      case _ =>
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
      new StringContext(prefix :: suffixes1.toList: _*).s(args1: _*)
    }
  }

  /** The `em` string interpolator works like the `i` string interpolator, but marks nonsensical errors
   *  using `<nonsensical>...</nonsensical>` tags.
   *  Note: Instead of these tags, it would be nicer to return a data structure containing the message string
   *  and a boolean indicating whether the message is sensical, but then we cannot use string operations
   *  like concatenation, stripMargin etc on the values returned by em"...", and in the current error
   *  message composition methods, this is crucial.
   */
  class ErrorMessageFormatter(sc: StringContext) extends StringFormatter(sc):
    override protected def showArg(arg: Any)(using Context): String =
      wrapNonSensical(arg, super.showArg(arg)(using errorMessageCtx))

  private def wrapNonSensical(arg: Any, str: String)(using Context): String = {
    import Message._
    def isSensical(arg: Any): Boolean = arg match {
      case tpe: Type =>
        tpe.exists && !tpe.isErroneous
      case sym: Symbol if sym.isCompleted =>
        sym.info match {
          case _: ErrorType | TypeAlias(_: ErrorType) | NoType => false
          case _ => true
        }
      case _ => true
    }

    if (isSensical(arg)) str
    else nonSensicalStartTag + str + nonSensicalEndTag
  }

  private type Recorded = Symbol | ParamRef | SkolemType

  private case class SeenKey(str: String, isType: Boolean)
  private class Seen extends mutable.HashMap[SeenKey, List[Recorded]] {

    override def default(key: SeenKey) = Nil

    def record(str: String, isType: Boolean, entry: Recorded)(using Context): String = {

      /** If `e1` is an alias of another class of the same name, return the other
       *  class symbol instead. This normalization avoids recording e.g. scala.List
       *  and scala.collection.immutable.List as two different types
       */
      def followAlias(e1: Recorded): Recorded = e1 match {
        case e1: Symbol if e1.isAliasType =>
          val underlying = e1.typeRef.underlyingClassRef(refinementOK = false).typeSymbol
          if (underlying.name == e1.name) underlying else e1
        case _ => e1
      }
      val key = SeenKey(str, isType)
      val existing = apply(key)
      lazy val dealiased = followAlias(entry)

      // alts: The alternatives in `existing` that are equal, or follow (an alias of) `entry`
      var alts = existing.dropWhile(alt => dealiased ne followAlias(alt))
      if (alts.isEmpty) {
        alts = entry :: existing
        update(key, alts)
      }
      val suffix = alts.length match {
        case 1 => ""
        case n => n.toString.toCharArray.map {
          case '0' => '⁰'
          case '1' => '¹'
          case '2' => '²'
          case '3' => '³'
          case '4' => '⁴'
          case '5' => '⁵'
          case '6' => '⁶'
          case '7' => '⁷'
          case '8' => '⁸'
          case '9' => '⁹'
        }.mkString
      }
      str + suffix
    }
  }

  private class ExplainingPrinter(seen: Seen)(_ctx: Context) extends RefinedPrinter(_ctx) {

    /** True if printer should a source module instead of its module class */
    private def useSourceModule(sym: Symbol): Boolean =
      sym.is(ModuleClass, butNot = Package) && sym.sourceModule.exists && !_ctx.settings.YdebugNames.value

    override def simpleNameString(sym: Symbol): String =
      if (useSourceModule(sym)) simpleNameString(sym.sourceModule)
      else seen.record(super.simpleNameString(sym), sym.isType, sym)

    override def ParamRefNameString(param: ParamRef): String =
      seen.record(super.ParamRefNameString(param), param.isInstanceOf[TypeParamRef], param)

    override def toTextRef(tp: SingletonType): Text = tp match {
      case tp: SkolemType => seen.record(tp.repr.toString, isType = true, tp)
      case _ => super.toTextRef(tp)
    }

    override def toText(tp: Type): Text = tp match {
      case tp: TypeRef if useSourceModule(tp.symbol) => Str("object ") ~ super.toText(tp)
      case _ => super.toText(tp)
    }
  }

  /** Create explanation for single `Recorded` type or symbol */
  def explanation(entry: AnyRef)(using Context): String = {
    def boundStr(bound: Type, default: ClassSymbol, cmp: String) =
      if (bound.isRef(default)) "" else i"$cmp $bound"

    def boundsStr(bounds: TypeBounds): String = {
      val lo = boundStr(bounds.lo, defn.NothingClass, ">:")
      val hi = boundStr(bounds.hi, defn.AnyClass, "<:")
      if (lo.isEmpty) hi
      else if (hi.isEmpty) lo
      else s"$lo and $hi"
    }

    def addendum(cat: String, info: Type): String = info match {
      case bounds @ TypeBounds(lo, hi) if bounds ne TypeBounds.empty =>
        if (lo eq hi) i" which is an alias of $lo"
        else i" with $cat ${boundsStr(bounds)}"
      case _ =>
        ""
    }

    entry match {
      case param: TypeParamRef =>
        s"is a type variable${addendum("constraint", TypeComparer.bounds(param))}"
      case param: TermParamRef =>
        s"is a reference to a value parameter"
      case sym: Symbol =>
        val info =
          if (ctx.gadt.contains(sym))
            sym.info & ctx.gadt.fullBounds(sym)
          else
            sym.info
        s"is a ${ctx.printer.kindString(sym)}${sym.showExtendedLocation}${addendum("bounds", info)}"
      case tp: SkolemType =>
        s"is an unknown value of type ${tp.widen.show}"
    }
  }

  /** Turns a `Seen` into a `String` to produce an explanation for types on the
    * form `where: T is...`
    *
    * @return string disambiguating types
    */
  private def explanations(seen: Seen)(using Context): String = {
    def needsExplanation(entry: Recorded) = entry match {
      case param: TypeParamRef => ctx.typerState.constraint.contains(param)
      case param: ParamRef     => false
      case skolem: SkolemType => true
      case sym: Symbol =>
        ctx.gadt.contains(sym) && ctx.gadt.fullBounds(sym) != TypeBounds.empty
    }

    val toExplain: List[(String, Recorded)] = seen.toList.flatMap { kvs =>
      val res: List[(String, Recorded)] = kvs match {
        case (key, entry :: Nil) =>
          if (needsExplanation(entry)) (key.str, entry) :: Nil else Nil
        case (key, entries) =>
          for (alt <- entries) yield {
            val tickedString = seen.record(key.str, key.isType, alt)
            (tickedString, alt)
          }
      }
      res // help the inferrencer out
    }.sortBy(_._1)

    def columnar(parts: List[(String, String)]): List[String] = {
      lazy val maxLen = parts.map(_._1.length).max
      parts.map {
        case (leader, trailer) =>
          val variable = hl(leader)
          s"""$variable${" " * (maxLen - leader.length)} $trailer"""
      }
    }

    val explainParts = toExplain.map { case (str, entry) => (str, explanation(entry)) }
    val explainLines = columnar(explainParts)
    if (explainLines.isEmpty) "" else i"where:    $explainLines%\n          %\n"
  }

  private def errorMessageCtx(using Context): Context =
    ctx.property(MessageLimiter) match
      case Some(_: ErrorMessageLimiter) => ctx
      case _ => ctx.fresh.setProperty(MessageLimiter, ErrorMessageLimiter())

  /** Context with correct printer set for explanations */
  private def explainCtx(seen: Seen)(using Context): Context =
    val ectx = errorMessageCtx
    ectx.printer match
      case dp: ExplainingPrinter =>
        ectx // re-use outer printer and defer explanation to it
      case _ =>
        ectx.fresh.setPrinterFn(ctx => new ExplainingPrinter(seen)(ctx))

  /** Entrypoint for explanation string interpolator:
    *
    * ```
    * ex"disambiguate $tpe1 and $tpe2"
    * ```
    */
  def explained(op: Context ?=> String)(using Context): String = {
    val seen = new Seen
    val msg = op(using explainCtx(seen))
    val addendum = explanations(seen)
    if (addendum.isEmpty) msg else msg ++ "\n\n" ++ addendum
  }

  /** When getting a type mismatch it is useful to disambiguate placeholders like:
    *
    * ```
    * found:    List[Int]
    * required: List[T]
    * where:    T is a type in the initializer of value s which is an alias of
    *           String
    * ```
    *
    * @return the `where` section as well as the printing context for the
    *         placeholders - `("T is a...", printCtx)`
    */
  def disambiguateTypes(args: Type*)(using Context): (String, Context) = {
    val seen = new Seen
    val printCtx = explainCtx(seen)
    args.foreach(_.show(using printCtx)) // showing each member will put it into `seen`
    (explanations(seen), printCtx)
  }

  /** This method will produce a colored type diff from the given arguments.
    * The idea is to do this for known cases that are useful and then fall back
    * on regular syntax highlighting for the cases which are unhandled.
    *
    * Please not that if used in combination with `disambiguateTypes` the
    * correct `Context` for printing should also be passed when calling the
    * method.
    *
    * @return the (found, expected, changePercentage) with coloring to
    *         highlight the difference
    */
  def typeDiff(found: Type, expected: Type)(using Context): (String, String) = {
    val fnd = wrapNonSensical(found, found.show)
    val exp = wrapNonSensical(expected, expected.show)

    DiffUtil.mkColoredTypeDiff(fnd, exp) match {
      case _ if ctx.settings.color.value == "never" => (fnd, exp)
      case (fnd, exp, change) if change < 0.5 => (fnd, exp)
      case _ => (fnd, exp)
    }
  }

  /** Explicit syntax highlighting */
  def hl(s: String)(using Context): String =
    SyntaxHighlighting.highlight(s)

  /** Explicitly highlight a string with the same formatting as used for keywords */
  def hlAsKeyword(str: String)(using Context): String =
    if str.isEmpty || ctx.settings.color.value == "never" then str
    else s"${SyntaxHighlighting.KeywordColor}$str${SyntaxHighlighting.NoColor}"
}
