/** These tests test all the possible formats the f interpolator has to deal with.
 *
 *  The tests are sorted by argument category as the arguments are on https://docs.oracle.com/javase/6/docs/api/java/util/Formatter.html#detail
 *
 *  Some tests come from https://github.com/lampepfl/dotty/pull/3894/files
 */
object Test {
  def main(args: Array[String]) = {
    println(f"integer: ${5}%d")
    println(f"string: ${"l"}%s")
    println(f"${5}%s, ${6}%d, ${"hello"}%s")
    println(f"${3.14}%.2f rounds to ${3}%d")

    val x = 5
    println(f"$x%d")
    println(f"${x + 1}%d")
    multilineTests
    generalArgsTests
    characterArgsTests
    integralArgsTests
    floatingPointArgsTests
    dateArgsTests
    specificLiteralsTests
    argumentsTests
    unitTests
  }

  def multilineTests = {
    def test1(n: Int) = {
      println(f"""Bob is $n%2d years old""")
      println(f"""Bob will be ${n+1}%2d years old""")
      println(f"""$n%d+1 = ${n+1}%d""")
    }

    def test2(f: Float) = {
      println(f"""Best price: $f%.2f""")
      println(f"""$f%3.2f%% discount included""")
    }

    def test3(n: Int) = {
      val old = "old"
      val catcher: PartialFunction[Throwable, Unit] = { case e => println(e) }
      try { println(f"""Bob is ${s"$n"} years ${s"$old"}!""") } catch catcher
      try { println(f"""Bob is ${f"$n"} years ${s"$old"}!""") } catch catcher
      try { println(f"""Bob is ${f"$n%2d"} years ${s"$old"}!""") } catch catcher
      try { println(f"""Bob is ${s"$n%2d"} years ${s"$old"}!""") } catch catcher
    }

    test1(1)
    test1(12)
    test1(123)

    test2(10.0f)
    test2(13.345f)

    test3(1)
    println("===============")
    test3(12)
    println("===============")
    test3(123)
  }

  def generalArgsTests = {

    def booleanTest(b : Boolean) = println(f"The boolean is $b%b %<b")

    def hTest(arg : Int) = println(f"$arg%h")

    def stringTest(s : String) = println(f"The string is $s%s $s%s %1$$s")

    def noFormatHasStringDefault(s : String) = println(f"The string is $s")

    booleanTest(false)
    booleanTest(true)

    hTest(10)

    stringTest(null)
    stringTest("")
    stringTest("string1")

    noFormatHasStringDefault(null)
    noFormatHasStringDefault("")
    noFormatHasStringDefault("string2")
  }

  def characterArgsTests = {

    def charTest(c : Char) = println(f"The unicode character is $c%c")

    charTest('c')
  }

  def integralArgsTests = {

    def decimalIntegerTest(i : Int) = println(f"The decimal integer is $i%d, $i%2d, $i%d+1 = ${i+1}%d")

    def octalIntegerTest(i : Int) = println(f"The octal integer is $i%o")

    def hexadecimalIntegerTest(i : Int) = println(f"The hexadecimal integer is $i%x")

    decimalIntegerTest(2)

    octalIntegerTest(2)
    octalIntegerTest(8)

    hexadecimalIntegerTest(2)
    hexadecimalIntegerTest(32)
  }

  def floatingPointArgsTests = {
    def scientificNotationTest(f : Float) = println(f"The scientific notation is $f%e")

    def decimalFloatingPointTest(f : Float) = println(f"The decimal floating point is $f%f, $f%3.2f, $f%15.3f")

    def noPointFloatingPointTest(f : Float) = println(f"The decimal floating point is $f%2.0f")

    def gTest(f : Float) = println(f"The float value is $f%g")

    def aTest(f : Float) = println(f"The float value is $f%a")

    scientificNotationTest(2f)
    scientificNotationTest(0.543f)

    decimalFloatingPointTest(1234.5678f)

    noPointFloatingPointTest(10.0f)

    val i = Math.pow(10.0, -4).floatValue

    gTest(i)
    gTest(i/10.0f)

    aTest(-i/10.0f)
    aTest(+0f)
    aTest(Float.NaN)
    aTest(Float.PositiveInfinity)
    aTest(Float.NegativeInfinity)
  }

  /**
    * Note that dates may appear in English or in any other language. This means that they
    * depend on the place where the program is run. For that reason, some tests are commented,
    * to avoid this dependence.
  */
  def dateArgsTests = {
    import java.text.SimpleDateFormat
    import java.util.Locale
    import java.util.TimeZone

    val sdf = new SimpleDateFormat("dd-M-yyyy hh:mm:ss")
    val dateInString = "31-08-1982 10:20:56"
    val date = sdf.parse(dateInString)

    println(f"$date%tH")
    println(f"$date%tI")
    println(f"$date%tk")
    println(f"$date%tl")
    println(f"$date%tM")
    println(f"$date%tS")
    println(f"$date%tL")
    println(f"$date%tN")
    println(f"$date%tp")
    // println(f"$date%tz")
    // println(f"$date%tZ")
    // println(f"$date%ts")
    // println(f"$date%tQ")
    // println(f"$date%tB")
    // println(f"$date%tb")
    // println(f"$date%th")
    // println(f"$date%tA")
    // println(f"$date%ta")
    println(f"$date%tC")
    println(f"$date%tY")
    println(f"$date%ty")
    println(f"$date%tj")
    println(f"$date%tm")
    println(f"$date%td")
    println(f"$date%te")
    println(f"$date%tR")
    println(f"$date%tT")
    println(f"$date%tr")
    println(f"$date%tD")
    println(f"$date%tF")
    // println(f"$date%tc")
  }

  def specificLiteralsTests = {
    def percentArgsTest = println(f"the percentage is 100 %%")

    def lineSeparatorArgs = println(f"we have a line separator now %nand now, we are on the next line")

    def nothingTest = println(f"we have nothing")

    percentArgsTest
    lineSeparatorArgs
  }

  def argumentsTests = {
    println(f"${"a"}%s ${"b"}%s %<s")
  }

  def unitTests =
    val test = FormatInterpolatorTest()
    test.`f interpolator baseline`
    test.fIf
    test.fIfNot
}

import java.text.DecimalFormat

import scala.language.implicitConversions
import scala.util.chaining._

class FormatInterpolatorTest {
  import StringContextTestUtils.*
  implicit val stringToBoolean: Conversion[String, Boolean] = _.toBoolean
  implicit def stringToChar(s: String): Char = s(0)
  implicit def str2fmt(s: String): java.util.Formattable = new java.util.Formattable {
    def formatTo(f: java.util.Formatter, g: Int, w: Int, p: Int) = f.format("%s", s)
  }
  import java.util.{Calendar, Locale}
  val cal = Calendar.getInstance(Locale.US).tap(_.set(2012, Calendar.MAY, 26))
  implicit def strToDate(x: String): Calendar = Calendar.getInstance(Locale.US).tap(_.set(2012, Calendar.MAY, 26))

  def assertEquals(expected: String, actual: String): Unit =
    assert(expected == actual, s"[$expected] != [$actual]")

  def `f interpolator baseline`: Unit =

    val b_true  = true
    val b_false = false

    val i = 42

    val f_zero = 0.0
    val f_zero_- = -0.0

    val s = "Scala"

    val fff  = new java.util.Formattable {
      def formatTo(f: java.util.Formatter, g: Int, w: Int, p: Int) = f.format("4")
    }

    val ss = List[(String, String)] (
      // 'b' / 'B' (category: general)
      // -----------------------------
      f"${b_false}%b" -> "false",
      f"${b_true}%b"  -> "true",

      f"${null}%b"  -> "false",
      f"${false}%b" -> "false",
      f"${true}%b"  -> "true",
      f"${true && false}%b"                    -> "false",
      f"${java.lang.Boolean.valueOf(false)}%b" -> "false",
      f"${java.lang.Boolean.valueOf(true)}%b"  -> "true",

      f"${null}%B"  -> "FALSE",
      f"${false}%B" -> "FALSE",
      f"${true}%B"  -> "TRUE",
      f"${java.lang.Boolean.valueOf(false)}%B"  -> "FALSE",
      f"${java.lang.Boolean.valueOf(true)}%B"   -> "TRUE",

      f"${"true"}%b" -> "true",
      f"${"false"}%b"-> "false",

      // 'h' | 'H' (category: general)
      // -----------------------------
      f"${null}%h"   -> "null",
      f"${f_zero}%h"   -> "0",
      f"${f_zero_-}%h" -> "80000000",
      f"${s}%h"       -> "4c01926",

      f"${null}%H"  -> "NULL",
      f"${s}%H"       -> "4C01926",

      // 's' | 'S' (category: general)
      // -----------------------------
      f"${null}%s"  -> "null",
      f"${null}%S"  -> "NULL",
      f"${s}%s"     -> "Scala",
      f"${s}%S"     -> "SCALA",
      f"${5}"       -> "5",
      f"${i}"       -> "42",
      f"${Symbol("foo")}"    -> "Symbol(foo)",

      f"${Thread.State.NEW}" -> "NEW",

      // 'c' | 'C' (category: character)
      // -------------------------------
      f"${120:Char}%c"   -> "x",
      f"${120:Byte}%c"   -> "x",
      f"${120:Short}%c"  -> "x",
      f"${120:Int}%c"    -> "x",
      f"${java.lang.Character.valueOf('x')}%c"   -> "x",
      f"${java.lang.Byte.valueOf(120:Byte)}%c"   -> "x",
      f"${java.lang.Short.valueOf(120:Short)}%c" -> "x",
      f"${java.lang.Integer.valueOf(120)}%c"     -> "x",

      f"${'x' : java.lang.Character}%c"     -> "x",
      f"${(120:Byte) : java.lang.Byte}%c"   -> "x",
      f"${(120:Short) : java.lang.Short}%c" -> "x",
      f"${120 : java.lang.Integer}%c"       -> "x",

      f"${"Scala"}%c"   -> "S",

      // 'd' | 'o' | 'x' | 'X' (category: integral)
      // ------------------------------------------
      f"${120:Byte}%d"    -> "120",
      f"${120:Short}%d"   -> "120",
      f"${120:Int}%d"     -> "120",
      f"${120:Long}%d"    -> "120",
      f"${60 * 2}%d"      -> "120",
      f"${java.lang.Byte.valueOf(120:Byte)}%d"   -> "120",
      f"${java.lang.Short.valueOf(120:Short)}%d" -> "120",
      f"${java.lang.Integer.valueOf(120)}%d"     -> "120",
      f"${java.lang.Long.valueOf(120)}%d"        -> "120",
      f"${120 : java.lang.Integer}%d"        -> "120",
      f"${120 : java.lang.Long}%d"           -> "120",
      f"${BigInt(120)}%d"                    -> "120",

      f"${new java.math.BigInteger("120")}%d" -> "120",

      f"${4}%#10X" -> "       0X4",

      f"She is ${fff}%#s feet tall." -> "She is 4 feet tall.",

      f"Just want to say ${"hello, world"}%#s..." -> "Just want to say hello, world...",

      //{ implicit val strToShort: Conversion[String, Short] = java.lang.Short.parseShort ; f"${"120"}%d" } -> "120",
      //{ implicit val strToInt = (s: String) => 42 ; f"${"120"}%d" } -> "42",

      // 'e' | 'E' | 'g' | 'G' | 'f' | 'a' | 'A' (category: floating point)
      // ------------------------------------------------------------------
      f"${3.4f}%e" -> locally"3.400000e+00",
      f"${3.4}%e"  -> locally"3.400000e+00",
      f"${3.4f : java.lang.Float}%e" -> locally"3.400000e+00",
      f"${3.4 : java.lang.Double}%e" -> locally"3.400000e+00",

      f"${BigDecimal(3.4)}%e" -> locally"3.400000e+00",

      f"${new java.math.BigDecimal(3.4)}%e" -> locally"3.400000e+00",

      f"${3}%e"  -> locally"3.000000e+00",
      f"${3L}%e" -> locally"3.000000e+00",

      // 't' | 'T' (category: date/time)
      // -------------------------------
      f"${cal}%TD"                 -> "05/26/12",
      f"${cal.getTime}%TD"         -> "05/26/12",
      f"${cal.getTime.getTime}%TD" -> "05/26/12",
      f"""${"1234"}%TD"""        -> "05/26/12",

      // literals and arg indexes
      f"%%" -> "%",
      f" mind%n------%nmatter" ->
       """| mind
          |------
          |matter""".stripMargin.linesIterator.mkString(System.lineSeparator),
      f"${i}%d %<d ${9}%d"   -> "42 42 9",
      f"${7}%d %<d ${9}%d"   -> "7 7 9",
      f"${7}%d %2$$d ${9}%d" -> "7 9 9",

      f"${null}%d %<B" -> "null FALSE",

      f"${5: Any}"      -> "5",
      f"${5}%s%<d"      -> "55",
      f"${3.14}%s,%<f"  -> locally"3.14,${"3.140000"}",

      f"z" -> "z"
    )

    for ((f, s) <- ss) assertEquals(s, f)
  end `f interpolator baseline`

  def fIf =
    val res = f"${if true then 2.5 else 2.5}%.2f"
    val expected = locally"2.50"
    assertEquals(expected, res)

  def fIfNot =
    val res = f"${if false then 2.5 else 3.5}%.2f"
    val expected = locally"3.50"
    assertEquals(expected, res)

  // in Scala 2, [A >: Any] forced not to convert 3 to 3.0; Scala 3 harmonics should also respect lower bound.
  def fHeteroArgs() =
    val res = f"${3.14}%.2f rounds to ${3}%d"
    val expected = locally"${"3.14"} rounds to 3"
    assertEquals(expected, res)
}

object StringContextTestUtils:
  private val decimalSeparator: Char = new DecimalFormat().getDecimalFormatSymbols().getDecimalSeparator()
  private val numberPattern = """(\d+)\.(\d+.*)""".r
  private def applyProperLocale(number: String): String =
    val numberPattern(intPart, fractionalPartAndSuffix) = number
    s"$intPart$decimalSeparator$fractionalPartAndSuffix"

  extension (sc: StringContext)
    // Use this String interpolator to avoid problems with a locale-dependent decimal mark.
    def locally(numbers: String*): String =
      val numbersWithCorrectLocale = numbers.map(applyProperLocale)
      sc.s(numbersWithCorrectLocale: _*)

    // Handles cases like locally"3.14" - it's prettier than locally"${"3.14"}".
    def locally(): String = sc.parts.map(applyProperLocale).mkString
