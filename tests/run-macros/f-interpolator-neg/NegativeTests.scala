// import org.junit.Test
// import org.junit.Assert._

// import compilationAssertions._
// import Macro._

// /**
//   * These tests test some combinations that should make the f interpolator fail.
//   * They come from https://github.com/lampepfl/dotty/blob/master/tests/untried/neg/stringinterpolation_macro-neg.scala
//   */
// class NegativeTests {
//     val s = "Scala"
//     val d = 8
//     val b = false
//     val f = 3.14159
//     val c = 'c'
//     val t = new java.util.Date
//     val x = new java.util.Formattable {
//       def formatTo(ff: java.util.Formatter, g: Int, w: Int, p: Int): Unit = ff format "xxx"
//     }
//     val emptyString = ""
//     val is = " is "
//     val yo = "%2d years old"

//     @Test def numberArgumentsTest1() = {
//       assertNotCompile("new StringContext().f2()")
//     }

//     @Test def numberArgumentsTest2() = {
//       assertNotCompile("new StringContext(emptyString, is, yo).f2(s)")
//     }

//     @Test def numberArgumentsTest3() = {
//       assertNotCompile("new StringContext(emptyString, is, yo).f2(s, d, d)")
//     }

//     @Test def numberArgumentsTest4() = {
//       assertNotCompile("new StringContext(emptyString, emptyString).f2()")
//     }

//     @Test def interpolationMismatchTest1() = {
//       implicit val strToInt1 = (s: String) => 1
//       implicit val strToInt2 = (s: String) => 2
//       val string = "$s%d"
//       assertNotCompile("f2string")
//     }

//     @Test def interpolationMismatchTest2() = {
//       val string = "$s%b"
//       assertNotCompile("f2string")
//     }

//     @Test def interpolationMismatchTest3() = {
//       val string = "$s%i"
//       assertNotCompile("f2string")
//     }

//     @Test def interpolationMismatchTest4() = {
//       val string = "$s%c"
//       assertNotCompile("f2string")
//     }

//     @Test def interpolationMismatchTest5() = {
//       val string = "$f%c"
//       assertNotCompile("f2string")
//     }

//     @Test def interpolationMismatchTest6() = {
//       val string = "$s%x"
//       assertNotCompile("f2string")
//     }

//     @Test def interpolationMismatchTest7() = {
//       val string = "$b%d"
//       assertNotCompile("f2string")
//     }

//     @Test def interpolationMismatchTest8() = {
//       val string = "$s%d"
//       assertNotCompile("f2string")
//     }

//     @Test def interpolationMismatchTest9() = {
//       val string = "$f%o"
//       assertNotCompile("f2string")
//     }

//     @Test def interpolationMismatchTest10() = {
//       val string = "$s%e"
//       assertNotCompile("f2string")
//     }

//     @Test def interpolationMismatchTest11() = {
//       val string = "$b%f"
//       assertNotCompile("f2string")
//     }

//     @Test def flagMismatchTest1() = {
//       val string = "$s%+ 0,(s"
//       assertNotCompile("f2string")
//     }

//     @Test def flagMismatchTest2() = {
//       val string = "$c%#+ 0,(c"
//       assertNotCompile("f2string")
//     }

//     @Test def flagMismatchTest3() = {
//       val string = "$d%#d"
//       assertNotCompile("f2string")
//     }

//     @Test def flagMismatchTest4() = {
//       val string = "$d%,x"
//       assertNotCompile("f2string")
//     }

//     @Test def flagMismatchTest5() = {
//       val string = "$d%+ (x"
//       assertNotCompile("f2string")
//     }

//     @Test def flagMismatchTest6() = {
//       val string = "$f%,(a"
//       assertNotCompile("f2string")
//     }

//     @Test def flagMismatchTest7() = {
//       val string = "$t%#+ 0,(tT"
//       assertNotCompile("f2string")
//     }

//     @Test def badPrecisionTest1() = {
//       val string = "$c%.2c"
//       assertNotCompile("f2string")
//     }

//     @Test def badPrecisionTest2() = {
//       val string = "$d%.2d"
//       assertNotCompile("f2string")
//     }

//     @Test def badPrecisionTest3() = {
//       val string = "%.2%"
//       assertNotCompile("f2string")
//     }

//     @Test def badPrecisionTest4() = {
//       val string = "%.2n"
//       assertNotCompile("f2string")
//     }

//     @Test def badPrecisionTest5() = {
//       val string = "$f%.2a"
//       assertNotCompile("f2string")
//     }

//     @Test def badPrecisionTest6() = {
//       val string = "$t%.2tT"
//       assertNotCompile("f2string")
//     }

//     @Test def badIndexTest1() = {
//       val string = "%<s"
//       assertNotCompile("f2string")
//     }

//     @Test def badIndexTest2() = {
//       val string = "%<c"
//       assertNotCompile("f2string")
//     }

//     @Test def badIndexTest3() = {
//       val string = "%<tT"
//       assertNotCompile("f2string")
//     }

//     @Test def badIndexTest4() = {
//       val string = "${8}%d ${9}%d%3$$d"
//       assertNotCompile("f2string")
//     }

//     @Test def badIndexTest5() = {
//       val string = "${8}%d ${9}%d%0$$d"
//       assertNotCompile("f2string")
//     }

//     @Test def badIndexTest6() = {
//       val string = "${8}%d ${9}%d%3$$d"
//       assertNotCompile("f2string")
//     }

//     @Test def warningsTest1() = {
//       val string = "${8}%d ${9}%1$$d"
//       assertNotCompile("f2string")
//     }

//     @Test def warningsTest2() = {
//       val string = "$s%s $s%s %1$$<s"
//       assertNotCompile("f2string")
//     }

//     @Test def warningsTest3() = {
//       val string = "$s%s $s%1$$s"
//       assertNotCompile("f2string")
//     }

//     @Test def badArgTypesTest() = {
//       val string = "$s%#s"
//       assertNotCompile("f2string")
//     }

//     @Test def misunderstoodConversionTest1() = {
//       val string = "$t%tG"
//       assertNotCompile("f2string")
//     }

//     @Test def misunderstoodConversionTest2() = {
//       val string = "$t%t"
//       assertNotCompile("f2string")
//     }

//     @Test def misunderstoodConversionTest3() = {
//       val string = "$s%10.5"
//       assertNotCompile("f2string")
//     }

//     @Test def otherBrainFailuresTest() = {
//       val string = "${d}random-leading-junk%d"
//       assertNotCompile("f2string")
//     }
// }