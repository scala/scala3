package dotty.tools.backend.jvm

import scala.language.unsafeNulls

import org.junit.Assert._
import org.junit.Test

import scala.tools.asm
import asm._
import asm.tree._

import scala.tools.asm.Opcodes
import scala.jdk.CollectionConverters._
import Opcodes._

class TestBCode extends DottyBytecodeTest {
  import ASMConverters._
  @Test def nullChecks = {
    val source = """
                 |class Foo {
                 |  def foo(x: AnyRef): Int = {
                 |    val bool = x == null
                 |    if (x != null) 1
                 |    else 0
                 |  }
                 |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      val methodNode = getMethod(clsNode, "foo")
      correctNumberOfNullChecks(2, methodNode.instructions)
    }
  }

  @Test def byNameParameters = {
    val source = """
                   |class Foo {
                   |  def byNameParam(str: => String): Unit = {}
                   |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      val methodNode: MethodNode = getMethod(clsNode, "byNameParam")

      assert(methodNode.signature == "(Lscala/Function0<Ljava/lang/String;>;)V")
    }
  }

  /** This test verifies that simple matches are transformed if possible
   *  despite no annotation
   */
  @Test def basicTransformNonAnnotated = {
    val source = """
                 |object Foo {
                 |  def foo(i: Int) = i match {
                 |    case 2 => println(2)
                 |    case 1 => println(1)
                 |    case 0 => println(0)
                 |  }
                 |}""".stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")
      assert(verifySwitch(methodNode))
    }
  }

  /** This test verifies that simple matches with `@switch` annotations are
   *  indeed transformed to a switch
   */
  @Test def basicSwitch = {
    val source = """
                 |object Foo {
                 |  import scala.annotation.switch
                 |  def foo(i: Int) = (i: @switch) match {
                 |    case 2 => println(2)
                 |    case 1 => println(1)
                 |    case 0 => println(0)
                 |  }
                 |}""".stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")
      assert(verifySwitch(methodNode))
    }
  }

  @Test def switchWithAlternatives = {
    val source =
      """
        |object Foo {
        |  import scala.annotation.switch
        |  def foo(i: Int) = (i: @switch) match {
        |    case 2 => println(2)
        |    case 1 | 3 | 5 => println(1)
        |    case 0 => println(0)
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")
      assert(verifySwitch(methodNode))
    }
  }

  @Test def switchWithGuards = {
    val source =
      """
        |object Foo {
        |  import scala.annotation.switch
        |  def foo(i: Int, b: Boolean) = (i: @switch) match {
        |    case 2 => println(3)
        |    case 1 if b => println(2)
        |    case 1 => println(1)
        |    case 0 => println(0)
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")
      assert(verifySwitch(methodNode))
    }
  }

  @Test def switchOnStrings = {
    val source =
      """
        |object Foo {
        |  import scala.annotation.switch
        |  def foo(s: String) = s match {
        |    case "AaAa" => println(3)
        |    case "BBBB" | "c" => println(2)
        |    case "D" | "E" => println(1)
        |    case _ => println(0)
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")
      assert(verifySwitch(methodNode))
    }
  }

  @Test def matchWithDefaultNoThrowMatchError = {
    val source =
      """class Test {
        |  def test(s: String) = s match {
        |    case "Hello" => 1
        |    case _       => 2
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn = dir.lookupName("Test.class", directory = false)
      val clsNode = loadClassNode(clsIn.input)
      val method = getMethod(clsNode, "test")
      val throwMatchError = instructionsFromMethod(method).exists {
        case Op(Opcodes.ATHROW) => true
        case _ => false
      }
      assertFalse(throwMatchError)
    }
  }

  @Test def failTransform = {
    val source = """
                 |object Foo {
                 |  import scala.annotation.switch
                 |  def foo(i: Any) = (i: @switch) match {
                 |    case x: String => println("string!")
                 |    case x :: xs   => println("list!")
                 |  }
                 |}""".stripMargin
    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")

      assert(verifySwitch(methodNode, shouldFail = true))
    }
  }

  /** Make sure that creating multidim arrays reduces to "multinewarray"
   *  instruction
   */
  @Test def multidimArraysFromOfDim = {
    val source = """
                 |object Arr {
                 |  def arr = Array.ofDim[Int](2, 1)
                 |}""".stripMargin
    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Arr$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val method     = getMethod(moduleNode, "arr")

      val hadCorrectInstr =
        instructionsFromMethod(method)
        .collect {
          case x @ NewArray(op, _, dims)
            if op == Opcode.multianewarray && dims == 2 => x
        }
        .length > 0

      assert(hadCorrectInstr,
             "Did not contain \"multianewarray\" instruction in:\n" +
             instructionsFromMethod(method).mkString("\n"))
    }
  }

  @Test def arraysFromOfDim = {
    val source = """
                 |object Arr {
                 |  def arr1 = Array.ofDim[Int](2)
                 |  def arr2 = Array.ofDim[Unit](2)
                 |  def arr3 = Array.ofDim[String](2)
                 |  def arr4 = Array.ofDim[Map[String, String]](2)
                 |}""".stripMargin
    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Arr$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val arr1       = getMethod(moduleNode, "arr1")
      val arr2       = getMethod(moduleNode, "arr2")
      val arr3       = getMethod(moduleNode, "arr3")

      val arr1CorrectInstr =
        instructionsFromMethod(arr1)
        .collect {
          case x @ IntOp(op, oprnd)
            if op == Opcode.newarray && oprnd == Opcode.int => x
        }
        .length > 0

      assert(arr1CorrectInstr,
             "Did not contain \"multianewarray\" instruction in:\n" +
             instructionsFromMethod(arr1).mkString("\n"))

      val arr2CorrectInstr =
        instructionsFromMethod(arr2)
        .collect {
          case x @ TypeOp(op, oprnd)
            if op == Opcode.anewarray && oprnd == Opcode.boxedUnit => x
        }
        .length > 0

      assert(arr2CorrectInstr,
             "arr2 bytecode did not contain correct `anewarray` instruction:\n" +
             instructionsFromMethod(arr2)mkString("\n"))

      val arr3CorrectInstr =
        instructionsFromMethod(arr3)
        .collect {
          case x @ TypeOp(op, oprnd)
            if op == Opcode.anewarray && oprnd == Opcode.javaString => x
        }
        .length > 0

      assert(arr3CorrectInstr,
             "arr3 bytecode did not contain correct `anewarray` instruction:\n" +
             instructionsFromMethod(arr3).mkString("\n"))
    }
  }

  @Test def arraysFromDimAndFromNewEqual = {
    val source = """
                 |object Arr {
                 |  def arr1 = Array.ofDim[Int](2)
                 |  def arr2 = new Array[Int](2)
                 |}""".stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Arr$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val arr1       = getMethod(moduleNode, "arr1")
      val arr2       = getMethod(moduleNode, "arr2")

      // First two instructions of `arr1` fetch the static reference to `Array`
      val instructions1 = instructionsFromMethod(arr1).drop(2)
      val instructions2 = instructionsFromMethod(arr2)

      assert(instructions1 == instructions2,
        "Creating arrays using `Array.ofDim[Int](2)` did not equal bytecode for `new Array[Int](2)`\n" +
        diffInstructions(instructions1, instructions2))
    }
  }

  /** Verifies that arrays are not unnecessarily wrapped when passed to Java varargs methods */
  @Test def dontWrapArraysInJavaVarargs = {
    val source =
      """
        |import java.nio.file._
        |class Test {
        |  def test(xs: Array[String]) = {
        |     val p4 = Paths.get("Hello", xs: _*)
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val moduleIn = dir.lookupName("Test.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val method = getMethod(moduleNode, "test")

      val arrayWrapped = instructionsFromMethod(method).exists {
        case inv: Invoke => inv.name.contains("wrapRefArray")
        case _ => false
      }

      assert(!arrayWrapped, "Arrays should not be wrapped when passed to a Java varargs method\n")
    }
  }

  @Test def efficientTryCases = {
    val source =
      """
        |class Test {
        |  def test =
        |    try print("foo")
        |    catch {
        |      case _: scala.runtime.NonLocalReturnControl[_] => ()
        |    }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val moduleIn = dir.lookupName("Test.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val method = getMethod(moduleNode, "test")

      val hasInstanceof = instructionsFromMethod(method).exists {
        case TypeOp(Opcodes.INSTANCEOF, _) => true
        case _ => false
      }

      assert(!hasInstanceof, "Try case should not issue INSTANCEOF opcode\n")
    }
  }

  @Test def noBoxingInSyntheticEquals = {
    val source =
      """
        |case class Case(x: Long)
        |class Value(val x: Long) extends AnyVal
      """.stripMargin

    checkBCode(source) { dir =>
      for ((clsName, methodName) <- List(("Case", "equals"), ("Value$", "equals$extension"))) {
        val moduleIn = dir.lookupName(s"$clsName.class", directory = false)
        val moduleNode = loadClassNode(moduleIn.input)
        val equalsMethod = getMethod(moduleNode, methodName)

        val callsEquals = instructionsFromMethod(equalsMethod).exists {
            case i @ Invoke(_, _, "equals", _, _) => true
            case i => false
        }

        assert(!callsEquals, s"equals method should not be called in the definition of $clsName#$methodName\n")
      }
    }
  }

  // See #4430
  @Test def javaBridgesAreNotVisible = {
    val source =
      """
        |class Test {
        |  def test = (new java.lang.StringBuilder()).append(Array[Char](), 0, 0)
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      // We check the method call signature to make sure we don't call a Java bridge
      val clsIn = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val testMethod = getMethod(clsNode, "test")
      val instructions = instructionsFromMethod(testMethod)
      val containsExpectedCall = instructions.exists {
        case Invoke(_, "java/lang/StringBuilder", "append", "([CII)Ljava/lang/StringBuilder;", _) => true
        case _ => false
      }
      assertTrue(containsExpectedCall)
    }
  }

  @Test def partialFunctions = {
    val source =
      """object Foo {
        |  def magic(x: Int) = x
        |  val foo: PartialFunction[Int, Int] = { case x => magic(x) }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      // We test that the anonymous class generated for the partial function
      // holds the method implementations and does not use forwarders
      val clsIn = dir.lookupName("Foo$$anon$1.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val applyOrElse = getMethod(clsNode, "applyOrElse")
      val instructions = instructionsFromMethod(applyOrElse)
      val callMagic = instructions.exists {
        case Invoke(_, _, "magic", _, _) => true
        case _ => false
      }
      assertTrue(callMagic)
    }
  }

  @Test def i4172 = {
    val source =
      """class Test {
        |  inline def foo(first: Int*)(second: String = "") = {}
        |
        |  def test = {
        |    foo(1)()
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val moduleIn = dir.lookupName("Test.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val method = getMethod(moduleNode, "test")

      val fooInvoke = instructionsFromMethod(method).exists {
        case inv: Invoke => inv.name == "foo"
        case _ => false
      }

      assert(!fooInvoke, "foo should not be called\n")
    }
  }

  @Test def returnThrowInPatternMatch = {
    val source =
      """class Test {
        |  def test(a: Any): Int = {
        |    a match {
        |      case _: Test => ???
        |    }
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val moduleIn = dir.lookupName("Test.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val method = getMethod(moduleNode, "test")

      val instructions = instructionsFromMethod(method)
      val hasReturn = instructions.exists {
        case Op(Opcodes.RETURN) => true
        case _ => false
      }
      assertFalse(hasReturn)
    }
  }

  /** Test that type lambda applications are properly dealias */
  @Test def i5090 = {
    val source =
      """class Test {
        |  type T[X] = X
        |
        |  def test(i: T[Int]): T[Int] = i
        |  def ref(i: Int): Int = i
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val test    = getMethod(clsNode, "test")
      val ref     = getMethod(clsNode, "ref")

      val testInstructions = instructionsFromMethod(test)
      val refInstructions  = instructionsFromMethod(ref)

      assert(testInstructions == refInstructions,
        "`T[Int]` was not properly dealias" +
        diffInstructions(testInstructions, refInstructions))
    }
  }

  /** Test that the receiver of a call to a method with varargs is not unnecessarily lifted */
  @Test def i5191 = {
    val source =
      """class Test {
        |  def foo(args: String*): String = ""
        |  def self = this
        |
        |  def test = self.foo()
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val method  = getMethod(clsNode, "test")

      val liftReceiver = instructionsFromMethod(method).exists {
        case VarOp(Opcodes.ASTORE, _) => true // receiver lifted in local val
        case _ => false
      }
      assertFalse("Receiver of a call to a method with varargs is unnecessarily lifted",
        liftReceiver)
    }
  }

  /** Test that the size of the lazy val initialiazer is under a certain threshold
   *
   *  - Fix to #5340 reduced the size from 39 instructions to 34
   *  - Fix to #505  reduced the size from 34 instructions to 32
   */
  @Test def i5340 = {
    val source =
      """class Test {
        |  def test = {
        |    lazy val x = 1
        |    x
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val method  = getMethod(clsNode, "x$lzyINIT1$1")
      assertEquals(32, instructionsFromMethod(method).size)
    }
  }

  /** Test that synchronize blocks don't box */
  @Test def i505 = {
    val source =
      """class Test {
        |  def test: Int = synchronized(1)
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val method  = getMethod(clsNode, "test")

      val doBox = instructionsFromMethod(method).exists {
        case Invoke(_, _, name, _, _) =>
          name == "boxToInteger" || name == "unboxToInt"
        case _ =>
          false
      }
      assertFalse(doBox)
    }
  }

  /** Test that the size of lazy field accesors is under a certain threshold
   */
  @Test def lazyFields = {
    val sourceUnsafe =
      """import scala.annotation.threadUnsafe
        |
        |class Test {
        |  @threadUnsafe lazy val test = 1
        |}
      """.stripMargin

    val sourceSafe =
      """class Test {
        |  lazy val test = 1
        |}
      """.stripMargin

    checkBCode(sourceUnsafe) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val method  = getMethod(clsNode, "test")
      assertEquals(14, instructionsFromMethod(method).size)
    }

    checkBCode(sourceSafe) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val method  = getMethod(clsNode, "test")
      assertEquals(94, instructionsFromMethod(method).size)
    }
  }

  /* Test that vals in traits cause appropriate `releaseFence()` calls to be emitted. */

  private def checkReleaseFence(releaseFenceExpected: Boolean, outputClassName: String, source: String): Unit = {
    checkBCode(source) { dir =>
      val clsIn = dir.lookupName(outputClassName, directory = false)
      val clsNode = loadClassNode(clsIn.input)
      val method = getMethod(clsNode, "<init>")

      val hasReleaseFence = instructionsFromMethod(method).exists {
        case Invoke(_, _, "releaseFence", _, _) => true
        case _ => false
      }

      assertEquals(source, releaseFenceExpected, hasReleaseFence)
    }
  }

  @Test def testInsertReleaseFence(): Unit = {
    // An empty trait does not cause a releaseFence.
    checkReleaseFence(false, "Bar.class",
      """trait Foo {
        |}
        |class Bar extends Foo
      """.stripMargin)

    // A val in a class does not cause a releaseFence.
    checkReleaseFence(false, "Bar.class",
      """trait Foo {
        |}
        |class Bar extends Foo {
        |  val x: Int = 5
        |}
      """.stripMargin)

    // A val in a trait causes a releaseFence.
    checkReleaseFence(true, "Bar.class",
      """trait Foo {
        |  val x: Int = 5
        |}
        |class Bar extends Foo
      """.stripMargin)

    // The presence of a var in the trait does not invalidate the need for a releaseFence.
    // Also, indirect mixin.
    checkReleaseFence(true, "Bar.class",
      """trait Parent {
        |  val x: Int = 5
        |  var y: Int = 6
        |}
        |trait Foo extends Parent
        |class Bar extends Foo
      """.stripMargin)

    // The presence of a var in the class does not invalidate the need for a releaseFence.
    checkReleaseFence(true, "Bar.class",
      """trait Foo {
        |  val x: Int = 5
        |}
        |class Bar extends Foo {
        |  var y: Int = 6
        |}
      """.stripMargin)

    // When inheriting trait vals through a superclass, no releaseFence is inserted.
    checkReleaseFence(false, "Bar.class",
      """trait Parent {
        |  val x: Int = 5
        |  var y: Int = 6
        |}
        |class Foo extends Parent // releaseFence in Foo, but not in Bar
        |class Bar extends Foo
      """.stripMargin)

    // Various other stuff that do not cause a releaseFence.
    checkReleaseFence(false, "Bar.class",
      """trait Foo {
        |  var w: Int = 1
        |  final val x = 2
        |  def y: Int = 3
        |  lazy val z: Int = 4
        |
        |  def add(a: Int, b: Int): Int = a + b
        |}
        |class Bar extends Foo
      """.stripMargin)
  }

  /* Test that objects compile to *final* classes. */

  private def checkFinalClass(outputClassName: String, source: String) = {
    checkBCode(source) {
      dir =>
        val moduleIn   = dir.lookupName(outputClassName, directory = false)
        val moduleNode = loadClassNode(moduleIn.input)
        assert((moduleNode.access & Opcodes.ACC_FINAL) != 0)
    }
  }

  @Test def objectsAreFinal =
    checkFinalClass("Foo$.class", "object Foo")

  @Test def objectsInClassAreFinal =
    checkFinalClass("Test$Foo$.class",
      """class Test {
        |  object Foo
        |}
      """.stripMargin)

  @Test def objectsInObjsAreFinal =
    checkFinalClass("Test$Foo$.class",
      """object Test {
        |  object Foo
        |}
      """.stripMargin)

  @Test def objectsInObjDefAreFinal =
    checkFinalClass("Test$Foo$2$.class",
      """
        |object Test {
        |  def bar() = {
        |    object Foo
        |  }
        |}
      """.stripMargin)

  @Test def objectsInClassDefAreFinal =
    checkFinalClass("Test$Foo$2$.class",
      """
        |class Test {
        |  def bar() = {
        |    object Foo
        |  }
        |}
      """.stripMargin)

  @Test def objectsInObjValAreFinal =
    checkFinalClass("Test$Foo$2$.class",
      """
        |class Test {
        |  val bar = {
        |    object Foo
        |  }
        |}
      """.stripMargin)

  @Test def i5750 = {
    val source =
      """class Test {
        |  def foo: String = ""
        |  def test(cond: Boolean): Int = {
        |    if (cond) foo
        |    1
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val method  = getMethod(clsNode, "test")

      val boxUnit = instructionsFromMethod(method).exists {
        case Field(Opcodes.GETSTATIC, "scala/runtime/BoxedUnit", _, _) =>
          true
        case _ =>
          false
      }
      assertFalse(boxUnit)
    }
  }

  @Test def i3271 = {
    val source =
      """class Test {
        |  def test = {
        |    var x = 0
        |    while(x <= 5) {
        |      println(x)
        |      x += 1
        |    }
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val method  = getMethod(clsNode, "test")

      val instructions = instructionsFromMethod(method)

      val expected = List(
        Op(Opcodes.ICONST_0),
        VarOp(Opcodes.ISTORE, 1),
        Label(2),
        FrameEntry(1, List(1), List()),
        VarOp(Opcodes.ILOAD, 1),
        Op(Opcodes.ICONST_5),
        Jump(Opcodes.IF_ICMPGT, Label(13)),
        Field(Opcodes.GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
        VarOp(Opcodes.ILOAD, 1),
        Invoke(Opcodes.INVOKESTATIC, "scala/runtime/BoxesRunTime", "boxToInteger", "(I)Ljava/lang/Integer;", false),
        Invoke(Opcodes.INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
        Incr(Opcodes.IINC, 1, 1),
        Jump(Opcodes.GOTO, Label(2)),
        Label(13),
        FrameEntry(3, List(), List()),
        Op(Opcodes.RETURN))

      assert(instructions == expected,
        "`test` was not properly generated\n" + diffInstructions(instructions, expected))
    }
  }

  @Test def i5924b = {
    val source =
      """|import scala.annotation.static
         |trait Base
         |
         |object Base {
         |  @static val x = 10
         |  @static final val y = 10
         |  @static def f: Int = 30
         |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Base.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val f = getMethod(clsNode, "f")
      val x = getField(clsNode, "x")
      val y = getField(clsNode, "y")
      assert((f.access & Opcodes.ACC_STATIC) != 0)
      List(x, y).foreach { node =>
        assert((node.access & Opcodes.ACC_STATIC) != 0)
        assert((node.access & Opcodes.ACC_FINAL) != 0)
      }
    }
  }

  @Test def i5924c = {
    val source =
      """|import scala.annotation.static
         |class Base
         |
         |object Base {
         |  @static val x = 10
         |  @static final val y = 10
         |  @static var a = 10
         |  @static final var b = 10
         |  @static def f: Int = 30
         |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Base.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val f = getMethod(clsNode, "f")
      val x = getField(clsNode, "x")
      val y = getField(clsNode, "y")
      val a = getField(clsNode, "a")
      val b = getField(clsNode, "b")
      assert((f.access & Opcodes.ACC_STATIC) != 0)
      List(x, y).foreach { node =>
        assert((node.access & Opcodes.ACC_STATIC) != 0)
        assert((node.access & Opcodes.ACC_FINAL) != 0)
      }
      List(a, b).foreach { node =>
        assert((node.access & Opcodes.ACC_STATIC) != 0)
      }
    }
  }

  @Test def freshNames = {
    val sourceA =
      """|class A {
         |  def a1[T: Ordering]: Unit = {}
         |  def a2[T: Ordering]: Unit = {}
         |}
      """.stripMargin
    val sourceB =
      """|class B {
         |  def b1[T: Ordering]: Unit = {}
         |  def b2[T: Ordering]: Unit = {}
         |}
      """.stripMargin

    checkBCode(List(sourceA, sourceB)) { dir =>
      val clsNodeA = loadClassNode(dir.lookupName("A.class", directory = false).input, skipDebugInfo = false)
      val clsNodeB = loadClassNode(dir.lookupName("B.class", directory = false).input, skipDebugInfo = false)
      val a1 = getMethod(clsNodeA, "a1")
      val a2 = getMethod(clsNodeA, "a2")
      val b1 = getMethod(clsNodeB, "b1")
      val b2 = getMethod(clsNodeB, "b2")

      def assertParamName(mn: MethodNode, expectedName: String) = {
        val actualName = mn.localVariables.get(1).name
        assert(actualName == expectedName,
          s"Method ${mn.name} has parameter $actualName but expected $expectedName")
      }

      // The fresh name counter should be reset for every compilation unit
      assertParamName(a1, "evidence$1")
      assertParamName(a2, "evidence$2")
      assertParamName(b1, "evidence$1")
      assertParamName(b2, "evidence$2")
    }
  }


  @Test // wrong local variable table for methods containing while loops
  def t9179(): Unit = {
    val code =
      """class C {
        |  def t(): Unit = {
        |    var x = ""
        |    while (x != null) {
        |      foo()
        |      x = null
        |    }
        |    bar()
        |  }
        |  def foo(): Unit = ()
        |  def bar(): Unit = ()
        |}
      """.stripMargin
    checkBCode(code) { dir =>
      val c = loadClassNode(dir.lookupName("C.class", directory = false).input, skipDebugInfo = false)
      val t = getMethod(c, "t")
      val instructions = instructionsFromMethod(t)
      val isFrameLine = (x: Instruction) => x.isInstanceOf[FrameEntry] || x.isInstanceOf[LineNumber]
      // TODO: The same test in scalac uses different labels because their LineNumberTable isn't the same as ours,
      // this should be investigated.
      assertSameCode(instructions.filterNot(isFrameLine), List(
        Label(0), Ldc(LDC, ""), VarOp(ASTORE, 1),
        Label(5), VarOp(ALOAD, 1), Jump(IFNULL, Label(19)),
        Label(10), VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "foo", "()V", false), Label(14), Op(ACONST_NULL), VarOp(ASTORE, 1), Jump(GOTO, Label(5)),
        Label(19), VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "bar", "()V", false), Label(24), Op(RETURN), Label(26)))
      val labels = instructions collect { case l: Label => l }
      val x = convertMethod(t).localVars.find(_.name == "x").get
      assertEquals(x.start, labels(1))
      assertEquals(x.end, labels(5))
    }
  }

  @Test
  def getClazz: Unit = {
    val source = """
                 |class Foo {
                 |  def sideEffect(): Int = { println("hi"); 1 }
                 |  def before1: Class[Int] = sideEffect().getClass
                 |  def before2: Class[Int] = sideEffect().getClass[Int]
                 |  def after: Class[Int] = { sideEffect(); classOf[Int] }
                 |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      val before1    = instructionsFromMethod(getMethod(clsNode, "before1"))
      val before2    = instructionsFromMethod(getMethod(clsNode, "before2"))
      val after      = instructionsFromMethod(getMethod(clsNode, "after"))

      assert(before1 == after,
        "`before1` was not translated to the same bytecode as `after`\n" +
        diffInstructions(before1, after))
      assert(before2 == after,
        "`before2` was not translated to the same bytecode as `after`\n" +
        diffInstructions(before2, after))
    }
  }

  @Test
  def invocationReceivers(): Unit = {
    import Opcodes.*

    checkBCode(List(invocationReceiversTestCode.definitions("Object"))) { dir =>
      val c1 = loadClassNode(dir.lookupName("C1.class", directory = false).input)
      val c2 = loadClassNode(dir.lookupName("C2.class", directory = false).input)
      assertSameCode(getMethod(c1, "clone"), List(VarOp(ALOAD, 0), Invoke(INVOKESTATIC, "T", "clone$", "(LT;)Ljava/lang/Object;", true), Op(ARETURN)))
      assertInvoke(getMethod(c1, "f1"), "T", "clone")
      assertInvoke(getMethod(c1, "f2"), "T", "clone")
      assertInvoke(getMethod(c1, "f3"), "C1", "clone")
      assertInvoke(getMethod(c2, "f1"), "T", "clone")
      assertInvoke(getMethod(c2, "f2"), "T", "clone")
      assertInvoke(getMethod(c2, "f3"), "C1", "clone")
    }
    checkBCode(List(invocationReceiversTestCode.definitions("String"))) { dir =>
      val c1b = loadClassNode(dir.lookupName("C1.class", directory = false).input)
      val c2b = loadClassNode(dir.lookupName("C2.class", directory = false).input)
      val tb = loadClassNode(dir.lookupName("T.class", directory = false).input)
      val ub = loadClassNode(dir.lookupName("U.class", directory = false).input)

      def ms(c: ClassNode, n: String) = c.methods.asScala.toList.filter(_.name == n)
      assert(ms(tb, "clone").length == 1)
      assert(ms(ub, "clone").isEmpty)
      val List(c1Clone) = ms(c1b, "clone").filter(_.desc == "()Ljava/lang/Object;")
      assert((c1Clone.access | Opcodes.ACC_BRIDGE) != 0)
      assertSameCode(c1Clone, List(VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C1", "clone", "()Ljava/lang/String;", false), Op(ARETURN)))

      def iv(m: MethodNode) = getInstructions(c1b, "f1").collect({case i: Invoke => i})
      assertSameCode(iv(getMethod(c1b, "f1")), List(Invoke(INVOKEINTERFACE, "T", "clone", "()Ljava/lang/String;", true)))
      assertSameCode(iv(getMethod(c1b, "f2")), List(Invoke(INVOKEINTERFACE, "T", "clone", "()Ljava/lang/String;", true)))
      // invokeinterface T.clone in C1 is OK here because it is not an override of Object.clone (different signature)
      assertSameCode(iv(getMethod(c1b, "f3")), List(Invoke(INVOKEINTERFACE, "T", "clone", "()Ljava/lang/String;", true)))
    }
  }

  @Test
  def invocationReceiversProtected(): Unit = {
    // http://lrytz.github.io/scala-aladdin-bugtracker/displayItem.do%3Fid=455.html / 9954eaf
    // also https://github.com/scala/bug/issues/1430 / 0bea2ab (same but with interfaces)
    val aC =
      """package a;
        |/*package private*/ abstract class A {
        |  public int f() { return 1; }
        |  public int t;
        |}
      """.stripMargin
    val bC =
      """package a;
        |public class B extends A { }
      """.stripMargin
    val iC =
      """package a;
        |/*package private*/ interface I { int f(); }
      """.stripMargin
    val jC =
      """package a;
        |public interface J extends I { }
      """.stripMargin
    val cC =
      """package b
        |class C {
        |  def f1(b: a.B) = b.f
        |  def f2(b: a.B) = { b.t = b.t + 1 }
        |  def f3(j: a.J) = j.f
        |}
      """.stripMargin

    checkBCode(scalaSources = List(cC), javaSources = List(aC, bC, iC, jC)) { dir =>
      val clsIn   = dir.subdirectoryNamed("b").lookupName("C.class", directory = false).input
      val c = loadClassNode(clsIn)

      assertInvoke(getMethod(c, "f1"), "a/B", "f") // receiver needs to be B (A is not accessible in class C, package b)
      assertInvoke(getMethod(c, "f3"), "a/J", "f") // receiver needs to be J
    }
  }

  @Test
  def specialInvocationReceivers(): Unit = {
    val code =
      """class C {
        |  def f1(a: Array[String]) = a.clone()
        |  def f2(a: Array[Int]) = a.hashCode()
        |  def f3(n: Nothing) = n.hashCode()
        |  def f4(n: Null) = n.toString()
        |
        |}
      """.stripMargin
    checkBCode(code) { dir =>
      val c = loadClassNode(dir.lookupName("C.class", directory = false).input)

      assertInvoke(getMethod(c, "f1"), "[Ljava/lang/String;", "clone") // array descriptor as receiver
      assertInvoke(getMethod(c, "f2"), "java/lang/Object", "hashCode") // object receiver
      assertInvoke(getMethod(c, "f3"), "java/lang/Object", "hashCode")
      assertInvoke(getMethod(c, "f4"), "java/lang/Object", "toString")
    }
  }

  @Test
  def deprecation(): Unit = {
    val code =
      """@deprecated
        |class Test {
        |  @deprecated
        |  val v = 0
        |
        |  @deprecated
        |  var x = 0
        |
        |  @deprecated("do not use this function!")
        |  def f(): Unit = ()
        |}
      """.stripMargin

    checkBCode(code) { dir =>
      val c = loadClassNode(dir.lookupName("Test.class", directory = false).input)
      assert((c.access & Opcodes.ACC_DEPRECATED) != 0)
      assert((getMethod(c, "f").access & Opcodes.ACC_DEPRECATED) != 0)

      assert((getField(c, "v").access & Opcodes.ACC_DEPRECATED) != 0)
      assert((getMethod(c, "v").access & Opcodes.ACC_DEPRECATED) != 0)

      assert((getField(c, "x").access & Opcodes.ACC_DEPRECATED) != 0)
      assert((getMethod(c, "x").access & Opcodes.ACC_DEPRECATED) != 0)
      assert((getMethod(c, "x_$eq").access & Opcodes.ACC_DEPRECATED) != 0)
    }
  }

  @Test def vcElideAllocations = {
    val source =
      s"""class ApproxState(val bits: Int) extends AnyVal
         |class Foo {
         |  val FreshApprox: ApproxState = new ApproxState(4)
         |  var approx: ApproxState = FreshApprox
         |  def meth1: Boolean = approx == FreshApprox
         |  def meth2: Boolean = (new ApproxState(4): ApproxState) == FreshApprox
         |}
         """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      val meth1      = getMethod(clsNode, "meth1")
      val meth2      = getMethod(clsNode, "meth2")
      val instructions1 = instructionsFromMethod(meth1)
      val instructions2 = instructionsFromMethod(meth2)

      val isFrameLine = (x: Instruction) => x.isInstanceOf[FrameEntry] || x.isInstanceOf[LineNumber]

      // No allocations of ApproxState

      assertSameCode(instructions1.filterNot(isFrameLine), List(
        VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "Foo", "approx", "()I", false),
        VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "Foo", "FreshApprox", "()I", false),
        Jump(IF_ICMPNE, Label(7)), Op(ICONST_1),
        Jump(GOTO, Label(10)),
        Label(7), Op(ICONST_0),
        Label(10), Op(IRETURN)))

      assertSameCode(instructions2.filterNot(isFrameLine), List(
        Op(ICONST_4),
        VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "Foo", "FreshApprox", "()I", false),
        Jump(IF_ICMPNE, Label(6)), Op(ICONST_1),
        Jump(GOTO, Label(9)),
        Label(6), Op(ICONST_0),
        Label(9), Op(IRETURN)))
    }
  }
}

object invocationReceiversTestCode {
  // if cloneType is more specific than Object (e.g., String), a bridge method is generated.
  def definitions(cloneType: String): String =
    s"""trait T { override def clone(): $cloneType = "hi" }
        |trait U extends T
        |class C1 extends U with Cloneable {
        |  // The comments below are true when `cloneType` is Object.
        |  // C1 gets a forwarder for clone that invokes T.clone. this is needed because JVM method
        |  // resolution always prefers class members, so it would resolve to Object.clone, even if
        |  // C1 is a subtype of the interface T which has an overriding default method for clone.
        |
        |  // invokeinterface T.clone
        |  def f1 = (this: T).clone()
        |
        |  // cannot invokeinterface U.clone (NoSuchMethodError). Object.clone would work here, but
        |  // not in the example in C2 (illegal access to protected). T.clone works in all cases and
        |  // resolves correctly.
        |  def f2 = (this: U).clone()
        |
        |  // invokevirtual C1.clone()
        |  def f3 = (this: C1).clone()
        |}
        |
        |class C2 {
        |  def f1(t: T) = t.clone()  // invokeinterface T.clone
        |  def f2(t: U) = t.clone()  // invokeinterface T.clone -- Object.clone would be illegal (protected, explained in C1)
        |  def f3(t: C1) = t.clone() // invokevirtual C1.clone -- Object.clone would be illegal
        |}
    """.stripMargin
}
