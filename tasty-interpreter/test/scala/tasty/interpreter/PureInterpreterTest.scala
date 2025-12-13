package scala.tasty.interpreter

import java.io.{ByteArrayOutputStream, File, PrintStream}

import dotty.tools.dotc.core.Contexts
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.util.DiffUtil
import dotty.tools.io.Path

import scala.io.Source
import scala.util.Using
import scala.tasty.interpreter.pure.PureInterpreterInspector
import scala.tasty.inspector.TastyInspector

/**
 * Test suite for the Pure TASTy Interpreter.
 *
 * This tests interpretation WITHOUT JVM reflection - all code is executed
 * by interpreting TASTy trees.
 */
object PureInterpreterTest {

  def main(args: Array[String]): Unit = {
    println("=== Pure TASTy Interpreter Tests ===\n")

    var passed = 0
    var failed = 0

    // Test 1: Simple expressions
    if (testSimpleExpressions()) passed += 1 else failed += 1

    // Test 2: Match expressions
    if (testMatchExpressions()) passed += 1 else failed += 1

    // Test 3: Try/Catch
    if (testTryCatch()) passed += 1 else failed += 1

    // Test 4: Closures and lambdas
    if (testClosures()) passed += 1 else failed += 1

    // Test 5: Type patterns and extractors
    if (testExistingPrototype()) passed += 1 else failed += 1

    // Test 6: Try/catch
    if (testTryCatchExceptions()) passed += 1 else failed += 1

    // Test 7: List patterns
    if (testListPatterns()) passed += 1 else failed += 1

    // Test 8: Macro-like computations
    if (testMacroLikeComputations()) passed += 1 else failed += 1

    // Test 9: String interpolation
    if (testStringInterpolation()) passed += 1 else failed += 1

    // Test 10: By-name parameters
    if (testByNameParameters()) passed += 1 else failed += 1

    // Test 11: For-comprehensions
    if (testForComprehensions()) passed += 1 else failed += 1

    println(s"\n=== Results: $passed passed, $failed failed ===")
    if (failed > 0) sys.exit(1)
  }

  def testSimpleExpressions(): Boolean = {
    println("Test 1: Simple expressions")
    val source = """
      |object TestSimple {
      |  def main(args: Array[String]): Unit = {
      |    // Literals
      |    println(42)
      |    println("hello")
      |
      |    // Variables
      |    val x = 10
      |    var y = 20
      |    y = y + x
      |    println(y)
      |
      |    // If/else
      |    val z = if (x > 5) "big" else "small"
      |    println(z)
      |
      |    // While loop
      |    var i = 3
      |    while (i > 0) {
      |      println(i)
      |      i = i - 1
      |    }
      |  }
      |}
    """.stripMargin

    val expected = """42
                     |hello
                     |30
                     |big
                     |3
                     |2
                     |1
                     |""".stripMargin

    runTest("simple", source, expected)
  }

  def testMatchExpressions(): Boolean = {
    println("Test 2: Match expressions")
    val source = """
      |object TestMatch {
      |  def main(args: Array[String]): Unit = {
      |    // Literal patterns
      |    def describe(x: Int): String = x match {
      |      case 0 => "zero"
      |      case 1 => "one"
      |      case _ => "other"
      |    }
      |    println(describe(0))
      |    println(describe(1))
      |    println(describe(99))
      |
      |    // Guards
      |    def sign(x: Int): String = x match {
      |      case n if n < 0 => "negative"
      |      case n if n > 0 => "positive"
      |      case _ => "zero"
      |    }
      |    println(sign(-5))
      |    println(sign(10))
      |    println(sign(0))
      |  }
      |}
    """.stripMargin

    val expected = """zero
                     |one
                     |other
                     |negative
                     |positive
                     |zero
                     |""".stripMargin

    runTest("match", source, expected)
  }

  def testTryCatch(): Boolean = {
    println("Test 3: Block expressions")
    val source = """
      |object TestBlocks {
      |  def main(args: Array[String]): Unit = {
      |    // Simple block
      |    val a = {
      |      val x = 10
      |      val y = 20
      |      x + y
      |    }
      |    println(a)
      |
      |    // Nested blocks
      |    val b = {
      |      val outer = 5
      |      val inner = {
      |        val x = outer * 2
      |        x + 1
      |      }
      |      inner + outer
      |    }
      |    println(b)
      |
      |    // Block with method call
      |    def double(x: Int): Int = x * 2
      |    val c = {
      |      val temp = double(7)
      |      temp + 1
      |    }
      |    println(c)
      |  }
      |}
    """.stripMargin

    val expected = """30
                     |16
                     |15
                     |""".stripMargin

    runTest("blocks", source, expected)
  }

  def testClosures(): Boolean = {
    println("Test 4: Closures and lambdas")
    val source = """
      |object TestClosures {
      |  def main(args: Array[String]): Unit = {
      |    // Simple closure
      |    val add = (x: Int, y: Int) => x + y
      |    println(add(3, 4))
      |
      |    // Closure capturing environment
      |    val multiplier = 10
      |    val scale = (x: Int) => x * multiplier
      |    println(scale(5))
      |
      |    // Higher-order functions
      |    def twice(f: Int => Int, x: Int): Int = f(f(x))
      |    val increment = (x: Int) => x + 1
      |    println(twice(increment, 5))
      |
      |    // Closures with collections would go here when supported
      |  }
      |}
    """.stripMargin

    val expected = """7
                     |50
                     |7
                     |""".stripMargin

    runTest("closures", source, expected)
  }

  def testExistingPrototype(): Boolean = {
    println("Test 5: Type patterns and extractors")
    val source = """
      |object TestTypePatterns {
      |  def main(args: Array[String]): Unit = {
      |    // Type patterns
      |    def describe(x: Any): String = x match {
      |      case _: Int => "int"
      |      case _: String => "string"
      |      case _: Boolean => "boolean"
      |      case _ => "other"
      |    }
      |    println(describe(42))
      |    println(describe("hello"))
      |    println(describe(true))
      |    println(describe(3.14))
      |
      |    // Option extractors
      |    def optionValue(opt: Option[Int]): String = opt match {
      |      case Some(x) => "got " + x.toString
      |      case None => "empty"
      |    }
      |    println(optionValue(Some(10)))
      |    println(optionValue(None))
      |
      |    // Nested Option
      |    def nested(opt: Option[Option[Int]]): String = opt match {
      |      case Some(Some(x)) => "nested: " + x.toString
      |      case Some(None) => "inner empty"
      |      case None => "outer empty"
      |    }
      |    println(nested(Some(Some(5))))
      |    println(nested(Some(None)))
      |  }
      |}
    """.stripMargin

    val expected = """int
                     |string
                     |boolean
                     |other
                     |got 10
                     |empty
                     |nested: 5
                     |inner empty
                     |""".stripMargin

    runTest("typepatterns", source, expected)
  }

  def testTryCatchExceptions(): Boolean = {
    println("Test 6: Try/catch exceptions")
    val source = """
      |object TestTryCatch {
      |  def main(args: Array[String]): Unit = {
      |    // Try/catch with RuntimeException
      |    def safeDivide(a: Int, b: Int): String = {
      |      try {
      |        if (b == 0) throw new RuntimeException("division by zero")
      |        val result = a / b
      |        "result: " + result.toString
      |      } catch {
      |        case e: RuntimeException => "error: " + e.getMessage
      |      }
      |    }
      |    println(safeDivide(10, 2))
      |    println(safeDivide(10, 0))
      |
      |    // Try/finally
      |    var cleaned = false
      |    try {
      |      println("in try")
      |    } finally {
      |      cleaned = true
      |    }
      |    println("cleaned: " + cleaned.toString)
      |
      |    // Nested try/catch (IllegalArgumentException IS a RuntimeException)
      |    def nestedTry(): String = {
      |      try {
      |        try {
      |          throw new IllegalArgumentException("inner")
      |        } catch {
      |          case _: NullPointerException => "caught null"  // Won't match
      |        }
      |      } catch {
      |        case e: IllegalArgumentException => "caught illegal: " + e.getMessage
      |      }
      |    }
      |    println(nestedTry())
      |  }
      |}
    """.stripMargin

    val expected = """result: 5
                     |error: division by zero
                     |in try
                     |cleaned: true
                     |caught illegal: inner
                     |""".stripMargin

    runTest("trycatch", source, expected)
  }

  def testListPatterns(): Boolean = {
    println("Test 7: List patterns")
    val source = """
      |object TestListPatterns {
      |  def main(args: Array[String]): Unit = {
      |    // Build lists
      |    val xs = 1 :: 2 :: 3 :: Nil
      |    println("xs: " + xs.toString)
      |
      |    // Pattern matching with :: and Nil
      |    def describe(xs: List[Int]): String = xs match {
      |      case Nil => "nil"
      |      case h :: Nil => "single(" + h.toString + ")"
      |      case h :: t => "cons(" + h.toString + ", tail=" + t.length.toString + ")"
      |    }
      |    println(describe(Nil))
      |    println(describe(1 :: Nil))
      |    println(describe(xs))
      |
      |    // Recursive sum using pattern matching
      |    def sum(xs: List[Int]): Int = xs match {
      |      case Nil => 0
      |      case h :: t => h + sum(t)
      |    }
      |    println("sum: " + sum(xs).toString)
      |
      |    // Recursive length using pattern matching
      |    def len(xs: List[Int]): Int = xs match {
      |      case Nil => 0
      |      case _ :: t => 1 + len(t)
      |    }
      |    println("len: " + len(xs).toString)
      |  }
      |}
    """.stripMargin

    val expected = """xs: List(1, 2, 3)
                     |nil
                     |single(1)
                     |cons(1, tail=2)
                     |sum: 6
                     |len: 3
                     |""".stripMargin

    runTest("listpatterns", source, expected)
  }

  def testMacroLikeComputations(): Boolean = {
    println("Test 8: Macro-like computations")
    val source = """
      |object TestMacroLike {
      |  // Simulate power macro - recursive computation at compile-time
      |  def power(n: Int, x: Double): Double = {
      |    if (n == 0) 1.0
      |    else if (n == 1) x
      |    else if (n % 2 == 0) {
      |      val y = x * x
      |      power(n / 2, y)
      |    }
      |    else x * power(n - 1, x)
      |  }
      |
      |  // Simulate type inspection (like Type.of[T])
      |  def typeNameOf[T](value: T): String = value match {
      |    case _: Int => "Int"
      |    case _: String => "String"
      |    case _: Boolean => "Boolean"
      |    case _: Double => "Double"
      |    case _: List[?] => "List"
      |    case _ => "Unknown"
      |  }
      |
      |  // Simulate tree construction (like '{ ... })
      |  sealed trait Expr
      |  case class IntLit(value: Int) extends Expr
      |  case class Add(l: Expr, r: Expr) extends Expr
      |  case class Mul(l: Expr, r: Expr) extends Expr
      |
      |  def eval(e: Expr): Int = e match {
      |    case IntLit(v) => v
      |    case Add(l, r) => eval(l) + eval(r)
      |    case Mul(l, r) => eval(l) * eval(r)
      |  }
      |
      |  def main(args: Array[String]): Unit = {
      |    // Test power function
      |    println("power(0, 5): " + power(0, 5.0).toString)
      |    println("power(1, 5): " + power(1, 5.0).toString)
      |    println("power(2, 5): " + power(2, 5.0).toString)
      |    println("power(3, 5): " + power(3, 5.0).toString)
      |
      |    // Test type inspection
      |    println("type of 42: " + typeNameOf(42))
      |    println("type of hello: " + typeNameOf("hello"))
      |    println("type of true: " + typeNameOf(true))
      |
      |    // Test expression tree evaluation
      |    val expr = Add(Mul(IntLit(2), IntLit(3)), IntLit(4))
      |    println("eval (2*3)+4: " + eval(expr).toString)
      |  }
      |}
    """.stripMargin

    val expected = """power(0, 5): 1.0
                     |power(1, 5): 5.0
                     |power(2, 5): 25.0
                     |power(3, 5): 125.0
                     |type of 42: Int
                     |type of hello: String
                     |type of true: Boolean
                     |eval (2*3)+4: 10
                     |""".stripMargin

    runTest("macrolike", source, expected)
  }

  def testStringInterpolation(): Boolean = {
    println("Test 9: String interpolation")
    val source = """
      |object TestStringInterpolation {
      |  def main(args: Array[String]): Unit = {
      |    // Simple string concatenation
      |    val name = "world"
      |    val greeting = "Hello, " + name + "!"
      |    println(greeting)
      |
      |    // String interpolation with s""
      |    val x = 42
      |    val msg = s"The answer is $x"
      |    println(msg)
      |
      |    // String interpolation with expressions
      |    val a = 10
      |    val b = 20
      |    println(s"Sum of $a and $b is ${a + b}")
      |
      |    // Multiple interpolations
      |    val first = "John"
      |    val last = "Doe"
      |    println(s"Name: $first $last")
      |  }
      |}
    """.stripMargin

    val expected = """Hello, world!
                     |The answer is 42
                     |Sum of 10 and 20 is 30
                     |Name: John Doe
                     |""".stripMargin

    runTest("stringinterpolation", source, expected)
  }

  def testByNameParameters(): Boolean = {
    println("Test 10: By-name parameters")
    val source = """
      |object TestByName {
      |  def main(args: Array[String]): Unit = {
      |    // By-name parameter evaluation
      |    var counter = 0
      |    def increment(): Int = {
      |      counter = counter + 1
      |      counter
      |    }
      |
      |    // Should evaluate twice
      |    def twice(x: => Int): Int = x + x
      |    counter = 0
      |    val result = twice(increment())
      |    println(s"twice result: $result, counter: $counter")
      |
      |    // Should not evaluate
      |    def maybe(cond: Boolean, x: => String): String = {
      |      if (cond) x else "skipped"
      |    }
      |    var evaluated = false
      |    def expensive(): String = {
      |      evaluated = true
      |      "computed"
      |    }
      |    println(maybe(false, expensive()))
      |    println(s"evaluated: $evaluated")
      |
      |    // Should evaluate
      |    println(maybe(true, expensive()))
      |    println(s"evaluated after: $evaluated")
      |  }
      |}
    """.stripMargin

    val expected = """twice result: 3, counter: 2
                     |skipped
                     |evaluated: false
                     |computed
                     |evaluated after: true
                     |""".stripMargin

    runTest("byname", source, expected)
  }

  def testForComprehensions(): Boolean = {
    println("Test 11: For-comprehensions")
    val source = """
      |object TestForComprehensions {
      |  def main(args: Array[String]): Unit = {
      |    // Simple for-yield over list
      |    val xs = List(1, 2, 3)
      |    val doubled = for (x <- xs) yield x * 2
      |    println(doubled.toString)
      |
      |    // For-foreach
      |    for (x <- xs) {
      |      println(s"item: $x")
      |    }
      |
      |    // Nested for
      |    val pairs = for {
      |      x <- List(1, 2)
      |      y <- List("a", "b")
      |    } yield (x, y)
      |    println(pairs.toString)
      |
      |    // For with filter
      |    val evens = for (x <- List(1, 2, 3, 4, 5) if x % 2 == 0) yield x
      |    println(evens.toString)
      |
      |    // For over Option
      |    val opt: Option[Int] = Some(10)
      |    val optResult = for (x <- opt) yield x + 1
      |    println(optResult.toString)
      |  }
      |}
    """.stripMargin

    val expected = """List(2, 4, 6)
                     |item: 1
                     |item: 2
                     |item: 3
                     |List((1,a), (1,b), (2,a), (2,b))
                     |List(2, 4)
                     |Some(11)
                     |""".stripMargin

    runTest("forcomprehensions", source, expected)
  }

  private def runTest(name: String, source: String, expected: String): Boolean = {
    try {
      val out = java.nio.file.Paths.get(s"out/pure-interpreter-test-$name")
      if (!java.nio.file.Files.exists(out))
        java.nio.file.Files.createDirectories(out)

      // Write source to temp file
      val sourceFile = out.resolve(s"Test$name.scala")
      java.nio.file.Files.writeString(sourceFile, source)

      // Compile
      val reporter = new Reporter {
        // Level 2 = ERROR in interfaces.Diagnostic
        def doReport(dia: Diagnostic)(implicit ctx: Contexts.Context): Unit = {
          if (dia.level >= 2) println(s"  COMPILE ERROR: ${dia.message}")
        }
      }

      println(s"  Compiling $name...")
      val compileResult = dotty.tools.dotc.Main.process(
        Array("-classpath", System.getProperty("java.class.path"), "-d", out.toString, sourceFile.toString),
        reporter
      )

      // Find TASTy files
      val tastyFiles = dotty.tools.io.Path(out).walkFilter(_.extension == "tasty").map(_.toString).toList

      if (tastyFiles.isEmpty) {
        println(s"  FAILED: No TASTy files generated")
        return false
      }

      // Interpret
      println(s"  Interpreting...")
      val actualOutput = interpret(tastyFiles)

      if (actualOutput.trim == expected.trim) {
        println(s"  PASSED")
        true
      } else {
        println(s"  FAILED: Output mismatch")
        println("  Expected:")
        expected.linesIterator.foreach(l => println(s"    |$l"))
        println("  Actual:")
        actualOutput.linesIterator.foreach(l => println(s"    |$l"))
        false
      }
    } catch {
      case e: Exception =>
        println(s"  FAILED: ${e.getClass.getSimpleName}: ${e.getMessage}")
        e.printStackTrace()
        false
    }
  }

  private def interpret(tastyFiles: List[String]): String = {
    val ps = new ByteArrayOutputStream()
    try scala.Console.withOut(ps) {
      TastyInspector.inspectTastyFiles(tastyFiles)(new PureInterpreterInspector)
    } catch {
      case e: Throwable =>
        // Include output so far in exception
        throw new Exception(s"Interpreter error (output so far: ${ps.toString})", e)
    }
    // Filter out our diagnostic lines
    ps.toString.linesIterator.filterNot(_.startsWith("[PureInterpreter]")).mkString("\n") + "\n"
  }
}

