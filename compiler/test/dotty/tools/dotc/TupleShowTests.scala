package dotty.tools
package dotc

import core.*, Decorators.*, Symbols.*
import printing.Texts.*

import java.lang.System.{ lineSeparator => EOL }
import org.junit.Test

class TupleShowTests extends DottyTest:
  def IntType = defn.IntType
  def LongType = defn.LongType
  def ShortType = defn.ShortType
  def Types_10 = List.fill(5)(IntType) ::: List.fill(5)(LongType)
  def Types_20 = Types_10 ::: Types_10

  val tup0  = defn.tupleType(Nil)
  val tup1  = defn.tupleType(IntType :: Nil)
  val tup2  = defn.tupleType(IntType :: LongType :: Nil)
  val tup3  = defn.tupleType(IntType :: LongType :: ShortType :: Nil)
  val tup21 = defn.tupleType(Types_20 ::: IntType :: Nil)
  val tup22 = defn.tupleType(Types_20 ::: IntType :: LongType :: Nil)
  val tup23 = defn.tupleType(Types_20 ::: IntType :: LongType :: ShortType :: Nil)
  val tup24 = defn.tupleType(Types_20 ::: IntType :: LongType :: ShortType :: ShortType :: Nil)

  @Test def tup0_show  = chkEq("EmptyTuple.type", i"$tup0")
  @Test def tup1_show  = chkEq("Tuple1[Int]", i"$tup1")
  @Test def tup2_show  = chkEq("(Int, Long)", i"$tup2")
  @Test def tup3_show  = chkEq("(Int, Long, Short)", i"$tup3")
  @Test def tup21_show = chkEq(res21, i"$tup21")
  @Test def tup22_show = chkEq(res22, i"$tup22")
  @Test def tup23_show = chkEq(res23, i"$tup23")
  @Test def tup24_show = chkEq(res24, i"$tup24")

  @Test def tup3_text =
    val obt = tup3.toText(ctx.printer)
    val exp = Fluid(List(
      Str(")"),
      Str("Short"),
      Closed(List(Str(", "), Str("Long"))),
      Closed(List(Str(", "), Str("Int"))),
      Str("("),
    ))
    chkEq(exp, obt)

  @Test def tup3_layout10 =
    val obt = tup3.toText(ctx.printer).layout(10)
    val exp = Fluid(List(
      Str("  Short)"),
      Str("  Long, "),
      Str("(Int, "),
    ))
    chkEq(exp, obt)

  @Test def tup3_show10 = chkEq("(Int,\n  Long,\n  Short)".normEOL, tup3.toText(ctx.printer).mkString(10))

  val res21: String =
    """|(Int, Int, Int, Int, Int, Long, Long, Long, Long, Long, Int, Int, Int, Int,
       |  Int, Long, Long, Long, Long, Long, Int)""".stripMargin.normEOL

  val res22: String =
    """|(Int, Int, Int, Int, Int, Long, Long, Long, Long, Long, Int, Int, Int, Int,
       |  Int, Long, Long, Long, Long, Long, Int, Long)""".stripMargin.normEOL

  val res23: String =
    """|(Int, Int, Int, Int, Int, Long, Long, Long, Long, Long, Int, Int, Int, Int,
       |  Int, Long, Long, Long, Long, Long, Int, Long, Short)""".stripMargin.normEOL

  val res24: String =
    """|(Int, Int, Int, Int, Int, Long, Long, Long, Long, Long, Int, Int, Int, Int,
       |  Int, Long, Long, Long, Long, Long, Int, Long, Short, Short)""".stripMargin.normEOL

  def chkEq[A](expected: A, obtained: A) = assert(expected == obtained, diff(s"$expected", s"$obtained"))

  /** On Windows the string literal in this test source file will be read with `\n` (b/c of "-encoding UTF8")
  *   but the compiler will correctly emit \r\n as the line separator.
  *   So we align the expected result to faithfully compare test results. */
  extension (str: String) def normEOL: String = if EOL == "\n" then str else str.replace("\n", EOL)

  def diff(exp: String, obt: String) =
    val min = math.min(exp.length, obt.length)
    val pre =
      var i = 0
      while i < min && exp(i) == obt(i) do i += 1
      exp.take(i)
    val suf =
      val max = min - pre.length - 1
      var i = 0
      while i <= max && exp(exp.length - 1 - i) == obt(obt.length - 1 - i) do i += 1
      exp.drop(exp.length - 1)

    import scala.io.AnsiColor.*
    val ellip = BLACK + BOLD + "..." + RESET
    val compactPre = if pre.length <= 20 then pre else ellip + pre.drop(pre.length - 20)
    val compactSuf = if suf.length <= 20 then suf else suf.take(20) + ellip
    def extractDiff(s: String) = s.slice(pre.length, s.length - suf.length)
    s"""|Comparison Failure:
        |  expected: $compactPre${CYAN }${extractDiff(exp)}$RESET$compactSuf
        |  obtained: $compactPre$MAGENTA${extractDiff(obt)}$RESET$compactSuf
        |""".stripMargin
