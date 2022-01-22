package dotty.tools.dotc
package transform.localopt

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*

object FormatInterpolatorTransform:

  class PartsReporter(fun: Tree, args0: Tree, parts: List[Tree], args: List[Tree])(using Context) extends InterpolationReporter:
    private var reported = false
    private var oldReported = false
    private def partPosAt(index: Int, offset: Int) =
      val pos = parts(index).sourcePos
      pos.withSpan(pos.span.shift(offset))
    def partError(message: String, index: Int, offset: Int): Unit =
      reported = true
      report.error(message, partPosAt(index, offset))
    def partWarning(message: String, index: Int, offset: Int): Unit =
      reported = true
      report.warning(message, partPosAt(index, offset))
    def argError(message: String, index: Int): Unit =
      reported = true
      report.error(message, args(index).srcPos)
    def strCtxError(message: String): Unit =
      reported = true
      report.error(message, fun.srcPos)
    def argsError(message: String): Unit =
      reported = true
      report.error(message, args0.srcPos)
    def hasReported: Boolean = reported
    def resetReported(): Unit =
      oldReported = reported
      reported = false
    def restoreReported(): Unit = reported = oldReported
  end PartsReporter

  /** For f"${arg}%xpart", check format conversions and return (format, args)
   *  suitable for String.format(format, args).
   */
  def checked(fun: Tree, args0: Tree)(using Context): (Tree, Tree) =
    val (partsExpr, parts) = fun match
      case TypeApply(Select(Apply(_, (parts: SeqLiteral) :: Nil), _), _) =>
        (parts.elems, parts.elems.map { case Literal(Constant(s: String)) => s })
      case _ =>
        report.error("Expected statically known StringContext", fun.srcPos)
        (Nil, Nil)
    val (args, elemtpt) = args0 match
      case seqlit: SeqLiteral => (seqlit.elems, seqlit.elemtpt)
      case _ =>
        report.error("Expected statically known argument list", args0.srcPos)
        (Nil, EmptyTree)
    given reporter: InterpolationReporter = PartsReporter(fun, args0, partsExpr, args)

    def literally(s: String) = Literal(Constant(s))
    inline val skip = false
    if parts.lengthIs != args.length + 1 then
      reporter.strCtxError {
        if parts.isEmpty then "there are no parts"
        else s"too ${if parts.lengthIs > args.length + 1 then "few" else "many"} arguments for interpolated string"
      }
      (literally(""), args0)
    else if skip then
      val checked = parts.head :: parts.tail.map(p => if p.startsWith("%") then p else "%s" + p)
      (literally(checked.mkString), args0)
    else
      val checker = TypedFormatChecker(args)
      val (checked, cvs) = checker.checked(parts)
      if reporter.hasReported then (literally(parts.mkString), args0)
      else
        assert(checker.argc == checker.actuals.size, s"Expected ${checker.argc}, actuals size is ${checker.actuals.size} for [${parts.mkString(", ")}]")
        (literally(checked.mkString), SeqLiteral(checker.actuals.toList, elemtpt))
  end checked
end FormatInterpolatorTransform

/** This trait defines a tool to report errors/warnings that do not depend on Position. */
trait InterpolationReporter:

  /** Reports error/warning of size 1 linked with a part of the StringContext.
   *
   *  @param message the message to report as error/warning
   *  @param index the index of the part inside the list of parts of the StringContext
   *  @param offset the index in the part String where the error is
   *  @return an error/warning depending on the function
   */
  def partError(message: String, index: Int, offset: Int): Unit
  def partWarning(message: String, index: Int, offset: Int): Unit

  /** Reports error linked with an argument to format.
   *
   *  @param message the message to report as error/warning
   *  @param index the index of the argument inside the list of arguments of the format function
   *  @return an error depending on the function
   */
  def argError(message: String, index: Int): Unit

  /** Reports error linked with the list of arguments or the StringContext.
   *
   *  @param message the message to report in the error
   *  @return an error
   */
  def strCtxError(message: String): Unit
  def argsError(message: String): Unit

  /** Claims whether an error or a warning has been reported
   *
   *  @return true if an error/warning has been reported, false
   */
  def hasReported: Boolean

  /** Stores the old value of the reported and reset it to false */
  def resetReported(): Unit

  /** Restores the value of the reported boolean that has been reset */
  def restoreReported(): Unit
end InterpolationReporter
