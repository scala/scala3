package dotty.tools.backend.jvm

import org.junit.Assert._
import org.junit.Test

import scala.tools.asm.Opcodes

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
    checkFinalClass("Test$Foo$1$.class",
      """
        |object Test {
        |  def bar() = {
        |    object Foo
        |  }
        |}
      """.stripMargin)

  @Test def objectsInClassDefAreFinal =
    checkFinalClass("Test$Foo$1$.class",
      """
        |class Test {
        |  def bar() = {
        |    object Foo
        |  }
        |}
      """.stripMargin)

  @Test def objectsInObjValAreFinal =
    checkFinalClass("Test$Foo$1$.class",
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
        Jump(Opcodes.IF_ICMPGT, Label(16)),
        Field(Opcodes.GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
        VarOp(Opcodes.ILOAD, 1),
        Invoke(Opcodes.INVOKESTATIC, "scala/runtime/BoxesRunTime", "boxToInteger", "(I)Ljava/lang/Integer;", false),
        Invoke(Opcodes.INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
        VarOp(Opcodes.ILOAD, 1),
        Op(Opcodes.ICONST_1),
        Op(Opcodes.IADD),
        VarOp(Opcodes.ISTORE, 1),
        Jump(Opcodes.GOTO, Label(2)),
        Label(16),
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
}
