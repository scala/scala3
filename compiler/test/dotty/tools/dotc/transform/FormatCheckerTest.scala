package dotty.tools
package dotc.transform

import org.junit.{Test, Assert}, Assert.{assertEquals, assertFalse, assertTrue}

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}
import scala.util.chaining._

import java.util.{Calendar, Date, Formattable}

import localopt.{FormatChecker, StringContextChecker}

// TDD for just the Checker
class FormatCheckerTest:
  class TestReporter extends StringContextChecker.InterpolationReporter:
    private var reported = false
    private var oldReported = false
    val reports = ListBuffer.empty[(String, Int, Int)]

    def partError(message: String, index: Int, offset: Int): Unit =
      reported = true
      reports += ((message, index, offset))
    def partWarning(message: String, index: Int, offset: Int): Unit =
      reported = true
      reports += ((message, index, offset))
    def argError(message: String, index: Int): Unit =
      reported = true
      reports += ((message, index, 0))
    def strCtxError(message: String): Unit =
      reported = true
      reports += ((message, 0, 0))
    def argsError(message: String): Unit =
      reports += ((message, 0, 0))

    def hasReported: Boolean = reported

    def resetReported(): Unit =
      oldReported = reported
      reported = false

    def restoreReported(): Unit =
      reported = oldReported
  end TestReporter
  given TestReporter = TestReporter()

  /*
  enum ArgTypeTag:
    case BooleanArg, ByteArg, CharArg, ShortArg, IntArg, LongArg, FloatArg, DoubleArg, AnyArg,
         StringArg, FormattableArg, BigIntArg, BigDecimalArg, CalendarArg, DateArg
  given Conversion[ArgTypeTag, Int] = _.ordinal
  def argTypeString(tag: Int) =
    if tag < 0 then "Null"
    else if tag >= ArgTypeTag.values.length then throw RuntimeException(s"Bad tag $tag")
    else ArgTypeTag.values(tag)
  */

  class TestChecker(args: ClassTag[?]*)(using val reporter: TestReporter) extends FormatChecker:
    def argType(argi: Int, types: ClassTag[?]*): ClassTag[?] = types.find(_ == args(argi)).getOrElse(types.head)
    val argc = args.length

  def checked(parts: String*)(args: ClassTag[?]*): String =
    val checker = TestChecker(args*)
    val (amended, _) = checker.checked(parts.toList)
    assertFalse(checker.reporter.hasReported)
    amended.mkString
  def assertChecked(parts: String*)(args: ClassTag[?]*)(p: TestReporter => Boolean = _ => true): Unit =
    val checker = TestChecker(args*)
    checker.checked(parts.toList)
    assertTrue(p(checker.reporter))
  def errorIs(msg: String): (TestReporter => Boolean) = _.reports.head._1.contains(msg)

  @Test def `simple string` = assertEquals("xyz", checked("xyz")())
  @Test def `one string` = assertEquals("xyz%s123", checked("xyz", "123")(classTag[String]))
  @Test def `in first part` = assertEquals("x%ny%%z%s123", checked("x%ny%%z", "123")(classTag[String]))
  @Test def `one int` = assertEquals("xyz%d123", checked("xyz", "%d123")(classTag[Int]))
  //@Test def `one bad int`: Unit = assertChecked("xyz", "%d123")(classTag[String])(errorIs("Type error"))
  @Test def `extra descriptor` = assertChecked("xyz", "%s12%d3")(classTag[String])(errorIs("conversions must follow"))
  @Test def `bad leader`: Unit = assertChecked("%dxyz")()(_.reports.head._1.contains("conversions must follow"))
  @Test def `in second part`: Unit = assertEquals("xyz%s1%n2%%3", checked("xyz", "1%n2%%3")(classTag[String]))
  @Test def `something weird`: Unit = assertEquals("xyz%tH123", checked("xyz", "%tH123")(classTag[Calendar]))
