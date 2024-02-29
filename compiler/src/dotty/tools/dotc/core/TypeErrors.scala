package dotty.tools
package dotc
package core

import Types.*
import Symbols.*
import Flags.*
import Names.*
import Contexts.*
import SymDenotations.*
import Denotations.*
import Decorators.*
import reporting.*
import ast.untpd
import config.Printers.{cyclicErrors, noPrinter}

import scala.annotation.constructorOnly

abstract class TypeError(using creationContext: Context) extends Exception(""):

  /** Will the stack trace of this exception be filled in?
   *  This is expensive and only useful for debugging purposes.
   */
  def computeStackTrace: Boolean =
    ctx.debug
    || (cyclicErrors != noPrinter && this.isInstanceOf[CyclicReference] && !(ctx.mode is Mode.CheckCyclic))
    || ctx.settings.YdebugTypeError.value
    || ctx.settings.YdebugError.value
    || ctx.settings.YdebugUnpickling.value

  override def fillInStackTrace(): Throwable =
    if computeStackTrace then super.fillInStackTrace().nn
    else this

  /** Convert to message. This takes an additional Context, so that we
   *  use the context when the message is first produced, i.e. when the TypeError
   *  is handled. This makes a difference for CyclicErrors since we need to know
   *  the context where the completed symbol is referenced, but the creation
   *  context of the CyclicReference is the completion context for the symbol.
   *  See i2887b for a test case, where we want to see
   *  "recursive or overloaded method needs result type".
   */
  def toMessage(using Context): Message

  /** Uses creationContext to produce the message */
  override def getMessage: String = toMessage.message

object TypeError:
  def apply(msg: Message)(using Context) = new TypeError:
    def toMessage(using Context) = msg
end TypeError

class MalformedType(pre: Type, denot: Denotation, absMembers: Set[Name])(using Context) extends TypeError:
  def toMessage(using Context) = em"malformed type: $pre is not a legal prefix for $denot because it contains abstract type member${if (absMembers.size == 1) "" else "s"} ${absMembers.mkString(", ")}"

class MissingType(pre: Type, name: Name)(using Context) extends TypeError:
  private def otherReason(pre: Type)(using Context): String = pre match {
    case pre: ThisType if pre.cls.givenSelfType.exists =>
      i"\nor the self type of $pre might not contain all transitive dependencies"
    case _ => ""
  }

  override def toMessage(using Context): Message =
    if ctx.debug then printStackTrace()
    em"""cannot resolve reference to type $pre.$name
        |the classfile defining the type might be missing from the classpath${otherReason(pre)}"""
end MissingType

class RecursionOverflow(val op: String, details: => String, val previous: Throwable, val weight: Int)(using Context)
extends TypeError:

  def explanation: String = s"$op $details"

  private def recursions: List[RecursionOverflow] = {
    import scala.collection.mutable.ListBuffer
    val result = ListBuffer.empty[RecursionOverflow]
    @annotation.tailrec def loop(throwable: Throwable): List[RecursionOverflow] = throwable match {
      case ro: RecursionOverflow =>
        result += ro
        loop(ro.previous)
      case _ => result.toList
    }

    loop(this)
  }

  def opsString(rs: List[RecursionOverflow])(using Context): String = {
    val maxShown = 20
    if (rs.lengthCompare(maxShown) > 0)
      i"""${opsString(rs.take(maxShown / 2))}
         |  ...
         |${opsString(rs.takeRight(maxShown / 2))}"""
    else
      (rs.map(_.explanation): List[String]).mkString("\n  ", "\n|  ", "")
  }

  override def toMessage(using Context): Message =
    val mostCommon = recursions.groupBy(_.op).toList.maxBy(_._2.map(_.weight).sum)._2.reverse
    em"""Recursion limit exceeded.
        |Maybe there is an illegal cyclic reference?
        |If that's not the case, you could also try to increase the stacksize using the -Xss JVM option.
        |For the unprocessed stack trace, compile with -Yno-decode-stacktraces.
        |A recurring operation is (inner to outer):
        |${opsString(mostCommon).stripMargin}"""

  override def fillInStackTrace(): Throwable = this
  override def getStackTrace(): Array[StackTraceElement] = previous.getStackTrace().asInstanceOf
end RecursionOverflow

/** Post-process exceptions that might result from StackOverflow to add
  * tracing information while unwalking the stack.
  */
// Beware: Since this object is only used when handling a StackOverflow, this code
// cannot consume significant amounts of stack.
object handleRecursive:
  inline def underlyingStackOverflowOrNull(exc: Throwable): Throwable | Null =
    var e: Throwable | Null = exc
    while e != null && !e.isInstanceOf[StackOverflowError] do e = e.getCause
    e

  def apply(op: String, details: => String, exc: Throwable, weight: Int = 1)(using Context): Nothing =
    if ctx.settings.YnoDecodeStacktraces.value then
      throw exc
    else exc match
      case _: RecursionOverflow =>
        throw new RecursionOverflow(op, details, exc, weight)
      case _ =>
        val so = underlyingStackOverflowOrNull(exc)
        if so != null then throw new RecursionOverflow(op, details, so, weight)
        else throw exc
end handleRecursive

/**
 * This TypeError signals that completing denot encountered a cycle: it asked for denot.info (or similar),
 * so it requires knowing denot already.
 * @param denot
 */
class CyclicReference(val denot: SymDenotation)(using Context) extends TypeError:
  var inImplicitSearch: Boolean = false

  val cycleSym = denot.symbol

  // cycleSym.flags would try completing denot and would fail, but here we can use flagsUNSAFE to detect flags
  // set by the parser.
  def unsafeFlags = cycleSym.flagsUNSAFE
  def isMethod = unsafeFlags.is(Method)
  def isVal = !isMethod && cycleSym.isTerm

  override def toMessage(using Context): Message =
    val unsafeFlags = cycleSym.flagsUNSAFE
    val isMethod = unsafeFlags.is(Method)
    val isVal = !isMethod && cycleSym.isTerm

    /* This CyclicReference might have arisen from asking for `m`'s type while trying to infer it.
     * To try to diagnose this, walk the context chain searching for context in
     * Mode.InferringReturnType for the innermost member without type
     * annotations (!tree.tpt.typeOpt.exists).
     */
    def errorMsg(cx: Context): Message =
      if (cx.mode is Mode.InferringReturnType)
        cx.tree match {
          case tree: untpd.ValOrDefDef if !tree.tpt.typeOpt.exists =>
            if (inImplicitSearch)
              TermMemberNeedsResultTypeForImplicitSearch(cycleSym)
            else if (isMethod)
              OverloadedOrRecursiveMethodNeedsResultType(cycleSym)
            else if (isVal)
              RecursiveValueNeedsResultType(cycleSym)
            else
              errorMsg(cx.outer)
          case _ =>
            errorMsg(cx.outer)
        }

      // Give up and give generic errors.
      else if (cycleSym.isOneOf(GivenOrImplicitVal, butNot = Method) && cycleSym.owner.isTerm)
        CyclicReferenceInvolvingImplicit(cycleSym)
      else
        CyclicReferenceInvolving(denot)

    errorMsg(ctx)
  end toMessage

object CyclicReference:
  def apply(denot: SymDenotation)(using Context): CyclicReference =
    val ex = new CyclicReference(denot)
    if ex.computeStackTrace then
      cyclicErrors.println(s"Cyclic reference involving $denot")
      val sts = ex.getStackTrace.asInstanceOf[Array[StackTraceElement]]
      for (elem <- sts take 200)
        cyclicErrors.println(elem.toString)
    ex
end CyclicReference

class UnpicklingError(denot: Denotation, where: String, cause: Throwable)(using Context) extends TypeError:
  override def toMessage(using Context): Message =
    val debugUnpickling = cause match
      case cause: UnpicklingError => ""
      case _ =>
        if ctx.settings.YdebugUnpickling.value then
          cause.getStackTrace().nn.mkString("\n    ", "\n    ", "")
        else "\n\nRun with -Ydebug-unpickling to see full stack trace."
    em"""Could not read definition $denot$where. Caused by the following exception:
        |$cause$debugUnpickling"""
end UnpicklingError
