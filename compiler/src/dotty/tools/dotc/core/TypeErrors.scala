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
import util.Property
import config.Printers.{cyclicErrors, noPrinter}
import collection.mutable

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
    || ctx.settings.YdebugCyclic.value

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
  override def getMessage: String =
    try toMessage.message catch case ex: Throwable => "TypeError"

object TypeError:
  def apply(msg: Message)(using Context) = new TypeError:
    def toMessage(using Context) = msg
end TypeError

class MalformedType(pre: Type, denot: Denotation, absMembers: Set[Name])(using Context) extends TypeError:
  def toMessage(using Context) = em"malformed type: $pre is not a legal prefix for $denot because it contains abstract type member${if (absMembers.size == 1) "" else "s"} ${absMembers.mkString(", ")}"

class MissingType(val pre: Type, val name: Name)(using Context) extends TypeError:

  def reason(using Context): String =
    def missingClassFile =
      "The classfile defining the type might be missing from the classpath"
    val cls = pre.classSymbol
    val givenSelf = cls match
      case cls: ClassSymbol => cls.givenSelfType
      case _ => NoType
    pre match
      case pre: ThisType if pre.cls.givenSelfType.exists =>
        i"""$missingClassFile
           |or the self type of $pre might not contain all transitive dependencies"""
      case _ if givenSelf.exists && givenSelf.member(name).exists =>
        i"""$name exists as a member of the self type $givenSelf of $cls
           |but it cannot be called on a receiver whose type does not extend $cls"""
      case _ if pre.baseClasses.exists(_.findMember(name, pre, Private, EmptyFlags).exists) =>
        i"$name is a private member in a base class"
      case _ =>
        missingClassFile


  override def toMessage(using Context): Message =
    if ctx.debug then printStackTrace()
    em"""Cannot resolve reference to type $pre.$name.
        |$reason."""
end MissingType

class RecursionOverflow(val op: String, details: => String, val previous: Throwable, val weight: Int)(using Context)
extends TypeError:

  def explanation: String = s"$op $details"

  private def recursions: List[RecursionOverflow] = {
    val result = mutable.ListBuffer.empty[RecursionOverflow]
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
        |For the unprocessed stack trace, compile with -Xno-enrich-error-messages.
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
    if ctx.settings.XnoEnrichErrorMessages.value then
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
class CyclicReference(
    val denot: SymDenotation,
    val optTrace: Option[Array[CyclicReference.TraceElement]])(using Context)
extends TypeError:
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
              TermMemberNeedsResultTypeForImplicitSearch(this)
            else if (isMethod)
              OverloadedOrRecursiveMethodNeedsResultType(this)
            else if (isVal)
              RecursiveValueNeedsResultType(this)
            else
              errorMsg(cx.outer)
          case _ =>
            errorMsg(cx.outer)
        }

      // Give up and give generic errors.
      else if (cycleSym.isOneOf(GivenOrImplicitVal, butNot = Method) && cycleSym.owner.isTerm)
        CyclicReferenceInvolvingImplicit(this)
      else
        CyclicReferenceInvolving(this)

    errorMsg(ctx)
  end toMessage

object CyclicReference:

  def apply(denot: SymDenotation)(using Context): CyclicReference =
    val ex = new CyclicReference(denot, ctx.property(Trace).map(_.toArray))
    if ex.computeStackTrace then
      cyclicErrors.println(s"Cyclic reference involving $denot")
      val sts = ex.getStackTrace.asInstanceOf[Array[StackTraceElement]]
      for (elem <- sts take 200)
        cyclicErrors.println(elem.toString)
    ex

  type TraceElement = Context ?=> String
  type Trace = mutable.ArrayBuffer[TraceElement]
  val Trace = Property.Key[Trace]

  private def isTraced(using Context) =
    ctx.property(CyclicReference.Trace).isDefined

  private def pushTrace(info: TraceElement)(using Context): Unit =
    for buf <- ctx.property(CyclicReference.Trace) do
      buf += info

  private def popTrace()(using Context): Unit =
    for buf <- ctx.property(CyclicReference.Trace) do
      buf.dropRightInPlace(1)

  inline def trace[T](info: TraceElement)(inline op: => T)(using Context): T =
    val traceCycles = isTraced
    try
      if traceCycles then pushTrace(info)
      op
    finally
      if traceCycles then popTrace()

  inline def trace[T](prefix: String, sym: Symbol)(inline op: => T)(using Context): T =
    trace((ctx: Context) ?=> i"$prefix$sym")(op)
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
