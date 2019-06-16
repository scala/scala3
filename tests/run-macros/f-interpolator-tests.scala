/**
  * These tests test all the possible formats the f interpolator has to deal with.
  * The tests are sorted by argument category as the arguments are on https://docs.oracle.com/javase/6/docs/api/java/util/Formatter.html#detail
  *
  *
  * Some tests come from https://github.com/lampepfl/dotty/pull/3894/files
  */
object Test {
  def main(args: Array[String]) = {
    println(f"integer: ${5}%d")
    println(f"string: ${"l"}%s")
    println(f"${5}%s, ${6}%d, ${"hello"}%s")

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
}

