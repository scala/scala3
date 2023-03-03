package dotty.tools
package dotc
package transform
package init

import ast.tpd._
import core._
import util.Property
import util.SourcePosition
import Types._, Symbols._, Contexts._

import Trace.Trace

object Errors:
  private val IsFromPromotion = new Property.Key[Boolean]

  sealed trait Error:
    def trace: Trace
    def show(using Context): String

    def pos(using Context): SourcePosition = Trace.position(using trace).sourcePos

    def stacktrace(using Context): String =
      val preamble: String =
        if ctx.property(IsFromPromotion).nonEmpty
        then " Promotion trace:\n"
        else " Calling trace:\n"
      Trace.buildStacktrace(trace, preamble)

    def issue(using Context): Unit =
      report.warning(show, this.pos)
  end Error

  override def toString() = this.getClass.getName.nn

  /** Access non-initialized field */
  case class AccessNonInit(field: Symbol)(val trace: Trace) extends Error:
    def source: Tree = Trace.position(using trace)
    def show(using Context): String =
      "Access non-initialized " + field.show + "." + stacktrace

    override def pos(using Context): SourcePosition = field.sourcePos

  /** Promote a value under initialization to fully-initialized */
  case class PromoteError(msg: String)(val trace: Trace) extends Error:
    def show(using Context): String = msg + stacktrace

  case class AccessCold(field: Symbol)(val trace: Trace) extends Error:
    def show(using Context): String =
      "Access field " + field.show +  " on a cold object." + stacktrace

  case class CallCold(meth: Symbol)(val trace: Trace) extends Error:
    def show(using Context): String =
      "Call method " + meth.show + " on a cold object." + stacktrace

  case class CallUnknown(meth: Symbol)(val trace: Trace) extends Error:
    def show(using Context): String =
      val prefix = if meth.is(Flags.Method) then "Calling the external method " else "Accessing the external field"
      prefix + meth.show + " may cause initialization errors." + stacktrace

  /** Promote a value under initialization to fully-initialized */
  case class UnsafePromotion(msg: String, error: Error)(val trace: Trace) extends Error:
    def show(using Context): String =
      msg + stacktrace + "\n" +
        "Promoting the value to hot (transitively initialized) failed due to the following problem:\n" + {
          val ctx2 = ctx.withProperty(IsFromPromotion, Some(true))
          error.show(using ctx2)
        }

  /** Unsafe leaking a non-hot value as constructor arguments
   *
   *  Invariant: argsIndices.nonEmpty
   */
  case class UnsafeLeaking(error: Error, nonHotOuterClass: Symbol, argsIndices: List[Int])(val trace: Trace) extends Error:
    def show(using Context): String =
      "Problematic object instantiation: " + argumentInfo() + stacktrace + "\n" +
        "It leads to the following error during object initialization:\n" +
        error.show

    private def punctuation(i: Int): String =
      if i == argsIndices.size - 2 then " and "
      else if i == argsIndices.size - 1 then ""
      else ", "

    private def argumentInfo()(using Context): String =
      val multiple = argsIndices.size > 1 || nonHotOuterClass.exists
      val init =
        if nonHotOuterClass.exists
        then  "the outer " + nonHotOuterClass.name.show + ".this" + punctuation(-1)
        else ""

      val subject =
        argsIndices.zipWithIndex.foldLeft(init) { case (acc, (pos, i)) =>
          val text1 = "arg " + pos.toString
          val text2 = text1 + punctuation(i)
          acc + text2
        }
      val verb = if multiple then " are " else " is "
      val adjective = "not hot (transitively initialized)."
      subject + verb + adjective
