package dotty.tools.backend.jvm

import scala.language.unsafeNulls

import org.junit.Assert._
import org.junit.Test

import scala.tools.asm.Opcodes._

import scala.jdk.CollectionConverters._

class InlineBytecodeTests extends DottyBytecodeTest {
  import ASMConverters._
  @Test def inlineUnit = {
    val source = """
                 |class Foo {
                 |  inline def foo: Int = 1
                 |  inline def bar: Int = 1
                 |
                 |  def meth1: Unit = foo
                 |  def meth2: Unit = bar
                 |  def meth3: Unit = 1
                 |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      val meth1      = getMethod(clsNode, "meth1")
      val meth2      = getMethod(clsNode, "meth2")
      val meth3      = getMethod(clsNode, "meth3")

      val instructions1 = instructionsFromMethod(meth1)
      val instructions2 = instructionsFromMethod(meth2)
      val instructions3 = instructionsFromMethod(meth3)

      assert(instructions1 == instructions3,
        "`foo` was not properly inlined in `meth1`\n" +
        diffInstructions(instructions1, instructions3))

      assert(instructions2 == instructions3,
        "`bar` was not properly inlined in `meth2`\n" +
        diffInstructions(instructions2, instructions3))
    }
  }

  @Test def inlineAssert = {
    def mkSource(original: String, expected: String) =
      s"""
         |class Foo {
         |  def meth1: Unit = $original
         |  def meth2: Unit = $expected
         |}
         """.stripMargin

    val sources = List(
      mkSource("assert(true)", "()"),
      mkSource("assert(true, ???)", "()"),
      mkSource("assert(false)", "scala.runtime.Scala3RunTime.assertFailed()")
    )
    for (source <- sources)
      checkBCode(source) { dir =>
        val clsIn      = dir.lookupName("Foo.class", directory = false).input
        val clsNode    = loadClassNode(clsIn)
        val meth1      = getMethod(clsNode, "meth1")
        val meth2      = getMethod(clsNode, "meth2")

        val instructions1 = instructionsFromMethod(meth1)
        val instructions2 = instructionsFromMethod(meth2)

        assert(instructions1 == instructions2,
          "`assert` was not properly inlined in `meth1`\n" +
          diffInstructions(instructions1, instructions2))

      }
  }

  /** Disabled since locally comes from Predef now
  @Test
  def inlineLocally = {
    val source =
         """
         |class Foo {
         |  def meth1: Unit = locally {
         |    val a = 5
         |    a
         |  }
         |
         |  def meth2: Unit = {
         |    val a = 5
         |    a
         |  }
         |}
         """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      val meth1      = getMethod(clsNode, "meth1")
      val meth2      = getMethod(clsNode, "meth2")

      val instructions1 = instructionsFromMethod(meth1)
      val instructions2 = instructionsFromMethod(meth2)

      assert(instructions1 == instructions2,
        "`locally` was not properly inlined in `meth1`\n" +
        diffInstructions(instructions1, instructions2))
    }
  }
  */
/*
  @Test def inlineNn = {
    val source =
      s"""
         |class Foo {
         |  def meth1(x: Int | Null): Int = x.nn
         |  def meth2(x: Int | Null): Int = x.getClass; x
         |}
         """.stripMargin

    checkBCode(source) { dir =>
      val clsIn = dir.lookupName("Foo.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val meth1 = getMethod(clsNode, "meth1")
      val meth2 = getMethod(clsNode, "meth2")

      val instructions1 = instructionsFromMethod(meth1)
      val instructions2 = instructionsFromMethod(meth2)

      assert(instructions1 == instructions2,
        "`nn` was not properly inlined in `meth1`\n" +
        diffInstructions(instructions1, instructions2))
    }
  }
*/
  @Test def i4947 = {
    val source = """class Foo {
                   |  transparent inline def track[T](inline f: T): T = {
                   |    foo("tracking") // line 3
                   |    f // line 4
                   |  }
                   |  def main(args: Array[String]): Unit = { // line 6
                   |    track { // line 7
                   |      foo("abc") // line 8
                   |      track { // line 9
                   |        foo("inner") // line 10
                   |      }
                   |    } // line 11
                   |  }
                   |  def foo(str: String): Unit = ()
                   |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn, skipDebugInfo = false)

      val track = clsNode.methods.asScala.find(_.name == "track")
      assert(track.isEmpty, "method `track` should have been erased")

      val main = getMethod(clsNode, "main")
      val instructions = instructionsFromMethod(main)
      val expected =
        List(
          Label(0),
          LineNumber(6, Label(0)),
          LineNumber(3, Label(0)),
          VarOp(ALOAD, 0),
          Ldc(LDC, "tracking"),
          Invoke(INVOKEVIRTUAL, "Foo", "foo", "(Ljava/lang/String;)V", false),
          Label(6),
          LineNumber(8, Label(6)),
          VarOp(ALOAD, 0),
          Ldc(LDC, "abc"),
          Invoke(INVOKEVIRTUAL, "Foo", "foo", "(Ljava/lang/String;)V", false),
          Label(11),
          LineNumber(3, Label(11)),
          VarOp(ALOAD, 0),
          Ldc(LDC, "tracking"),
          Invoke(INVOKEVIRTUAL, "Foo", "foo", "(Ljava/lang/String;)V", false),
          Label(16),
          LineNumber(10, Label(16)),
          VarOp(ALOAD, 0),
          Ldc(LDC, "inner"),
          Invoke(INVOKEVIRTUAL, "Foo", "foo", "(Ljava/lang/String;)V", false),
          Op(RETURN),
          Label(22)
        )
      assert(instructions == expected,
        "`track` was not properly inlined in `main`\n" + diffInstructions(instructions, expected))

    }
  }

  @Test def i4947b = {
    val source = """class Foo {
                   |  transparent inline def track2[T](inline f: T): T = {
                   |    foo("tracking2") // line 3
                   |    f // line 4
                   |  }
                   |  transparent inline def track[T](inline f: T): T = {
                   |    foo("tracking") // line 7
                   |    track2 { // line 8
                   |      f // line 9
                   |    }
                   |  }
                   |  def main(args: Array[String]): Unit = { // line 12
                   |    track { // line 13
                   |      foo("abc") // line 14
                   |    }
                   |  }
                   |  def foo(str: String): Unit = ()
                   |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn, skipDebugInfo = false)

      val track = clsNode.methods.asScala.find(_.name == "track")
      assert(track.isEmpty, "method `track` should have been erased")

      val track2 = clsNode.methods.asScala.find(_.name == "track2")
      assert(track2.isEmpty, "method `track2` should have been erased")

      val main = getMethod(clsNode, "main")
      val instructions = instructionsFromMethod(main)
      val expected =
        List(
          Label(0),
          LineNumber(12, Label(0)),
          LineNumber(7, Label(0)),
          VarOp(ALOAD, 0),
          Ldc(LDC, "tracking"),
          Invoke(INVOKEVIRTUAL, "Foo", "foo", "(Ljava/lang/String;)V", false),
          Label(6),
          LineNumber(3, Label(6)),
          VarOp(ALOAD, 0),
          Ldc(LDC, "tracking2"),
          Invoke(INVOKEVIRTUAL, "Foo", "foo", "(Ljava/lang/String;)V", false),
          Label(11),
          LineNumber(14, Label(11)),
          VarOp(ALOAD, 0),
          Ldc(LDC, "abc"),
          Invoke(INVOKEVIRTUAL, "Foo", "foo", "(Ljava/lang/String;)V", false),
          Op(RETURN),
          Label(17)
        )
      assert(instructions == expected,
        "`track` was not properly inlined in `main`\n" + diffInstructions(instructions, expected))

    }
  }

  @Test def i4947c = {
    val source = """class Foo {
                   |  transparent inline def track2[T](inline f: T): T = {
                   |    foo("tracking2") // line 3
                   |    f // line 4
                   |  }
                   |  transparent inline def track[T](inline f: T): T = {
                   |    track2 { // line 7
                   |      foo("fgh") // line 8
                   |      f // line 9
                   |    }
                   |  }
                   |  def main(args: Array[String]): Unit = { // line 12
                   |    track { // line 13
                   |      foo("abc") // line 14
                   |    }
                   |  }
                   |  def foo(str: String): Unit = ()
                   |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn, skipDebugInfo = false)

      val track = clsNode.methods.asScala.find(_.name == "track")
      assert(track.isEmpty, "method `track` should have been erased")

      val track2 = clsNode.methods.asScala.find(_.name == "track2")
      assert(track2.isEmpty, "method `track2` should have been erased")

      val main = getMethod(clsNode, "main")
      val instructions = instructionsFromMethod(main)
      val expected =
        List(
          Label(0),
          LineNumber(12, Label(0)),
          LineNumber(3, Label(0)),
          VarOp(ALOAD, 0),
          Ldc(LDC, "tracking2"),
          Invoke(INVOKEVIRTUAL, "Foo", "foo", "(Ljava/lang/String;)V", false),
          Label(6),
          LineNumber(8, Label(6)),
          VarOp(ALOAD, 0),
          Ldc(LDC, "fgh"),
          Invoke(INVOKEVIRTUAL, "Foo", "foo", "(Ljava/lang/String;)V", false),
          Label(11),
          LineNumber(14, Label(11)),
          VarOp(ALOAD, 0),
          Ldc(LDC, "abc"),
          Invoke(INVOKEVIRTUAL, "Foo", "foo", "(Ljava/lang/String;)V", false),
          Op(RETURN),
          Label(17)
        )
      assert(instructions == expected,
        "`track` was not properly inlined in `main`\n" + diffInstructions(instructions, expected))

    }
  }

  @Test def i4947d = {
    val source = """class Foo {
                   |  transparent inline def track2[T](inline f: T): T = {
                   |    foo("tracking2") // line 3
                   |    f // line 4
                   |  }
                   |  transparent inline def track[T](inline f: T): T = {
                   |    track2 { // line 7
                   |      track2 { // line 8
                   |        f // line 9
                   |      }
                   |    }
                   |  }
                   |  def main(args: Array[String]): Unit = { // line 13
                   |    track { // line 14
                   |      foo("abc") // line 15
                   |    }
                   |  }
                   |  def foo(str: String): Unit = ()
                   |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn, skipDebugInfo = false)

      val track = clsNode.methods.asScala.find(_.name == "track")
      assert(track.isEmpty, "method `track` should have been erased")

      val track2 = clsNode.methods.asScala.find(_.name == "track2")
      assert(track2.isEmpty, "method `track2` should have been erased")

      val main = getMethod(clsNode, "main")
      val instructions = instructionsFromMethod(main)
      val expected =
        List(
          Label(0),
          LineNumber(13, Label(0)),
          LineNumber(3, Label(0)),
          VarOp(ALOAD, 0),
          Ldc(LDC, "tracking2"),
          Invoke(INVOKEVIRTUAL, "Foo", "foo", "(Ljava/lang/String;)V", false),
          Label(6),
          LineNumber(3, Label(6)),
          VarOp(ALOAD, 0),
          Ldc(LDC, "tracking2"),
          Invoke(INVOKEVIRTUAL, "Foo", "foo", "(Ljava/lang/String;)V", false),
          Label(11),
          LineNumber(15, Label(11)),
          VarOp(ALOAD, 0),
          Ldc(LDC, "abc"),
          Invoke(INVOKEVIRTUAL, "Foo", "foo", "(Ljava/lang/String;)V", false),
          Op(RETURN),
          Label(17)
        )
      assert(instructions == expected,
        "`track` was not properly inlined in `main`\n" + diffInstructions(instructions, expected))

    }
  }

    // Testing that a is not boxed
  @Test def i4522 = {
    val source = """class Foo {
                   |  def test: Int = {
                   |    var a = 10
                   |
                   |    transparent inline def f() = {
                   |      a += 1
                   |    }
                   |
                   |    f()
                   |    a
                   |  }
                   |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)

      val fun = getMethod(clsNode, "test")
      val instructions = instructionsFromMethod(fun)
      val expected =
        List(
          IntOp(BIPUSH, 10),
          VarOp(ISTORE, 1),
          Incr(IINC, 1, 1),
          VarOp(ILOAD, 1),
          Op(IRETURN),
        )
      assert(instructions == expected,
        "`f` was not properly inlined in `fun`\n" + diffInstructions(instructions, expected))

    }
  }

  @Test def i6375 = {
    val source = """class Test:
                   |  given Int = 0
                   |  def f(): Int ?=> Boolean = true : (Int ?=> Boolean)
                   |  transparent inline def g(): Int ?=> Boolean = true
                   |  def test = g()
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Test.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)

      val fun = getMethod(clsNode, "test")
      val instructions = instructionsFromMethod(fun)
      val expected =
        List(
          VarOp(ALOAD, 0),
          Invoke(INVOKEVIRTUAL, "Test", "given_Int", "()I", false),
          VarOp(ISTORE, 1),
          Op(ICONST_1),
          Op(IRETURN)
        )

      assert(instructions == expected,
        "`fg was not properly inlined in `test`\n" + diffInstructions(instructions, expected))

    }
  }

  @Test def i6800a = {
    val source = """class Foo:
                   |  inline def inlined(f: => Unit): Unit = f
                   |  def test: Unit = inlined { println("") }
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)

      val fun = getMethod(clsNode, "test")
      val instructions = instructionsFromMethod(fun)
      val expected = List(Invoke(INVOKESTATIC, "Foo", "f$proxy1$1", "()V", false), Op(RETURN))
      assert(instructions == expected,
        "`inlined` was not properly inlined in `test`\n" + diffInstructions(instructions, expected))

    }
  }

  @Test def i6800b = {
    val source = """class Foo:
                   |  inline def printIfZero(x: Int): Unit = inline x match
                   |    case 0 => println("zero")
                   |    case _ => ()
                   |  def test: Unit = printIfZero(0)
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)

      val fun = getMethod(clsNode, "test")
      val instructions = instructionsFromMethod(fun)
      val expected = List(
        Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
        Ldc(LDC, "zero"),
        Invoke(INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
        Op(RETURN)
      )
      assert(instructions == expected,
        "`printIfZero` was not properly inlined in `test`\n" + diffInstructions(instructions, expected))
    }
  }


  @Test def i9246 = {
    val source = """class Foo:
                   |  inline def check(v:Double): Unit = if(v==0) throw new Exception()
                   |  inline def divide(v: Double, d: Double): Double = { check(d); v / d }
                   |  def test =  divide(10,2)
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)

      val fun = getMethod(clsNode, "test")
      val instructions = instructionsFromMethod(fun)
      val expected = List(Ldc(LDC, 5.0), Op(DRETURN))
      assert(instructions == expected,
        "`divide` was not properly inlined in `test`\n" + diffInstructions(instructions, expected))
    }
  }

  @Test def finalVals = {
    val source = """class Test:
                   |  final val a = 1 // should be inlined but not erased
                   |  inline val b = 2 // should be inlined and erased
                   |  def test: Int = a + b
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Test.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)

      val fun = getMethod(clsNode, "test")
      val instructions = instructionsFromMethod(fun)
      val expected = List(Op(ICONST_3), Op(IRETURN))
      assert(instructions == expected,
        "`a and b were not properly inlined in `test`\n" + diffInstructions(instructions, expected))

      val methods = clsNode.methods.asScala.toList.map(_.name)
      assert(methods == List("<init>", "a", "test"), clsNode.methods.asScala.toList.map(_.name))
    }
  }


  @Test def i9466 = {
    val source = """class Test:
                   |  inline def i(inline f: Int => Boolean): String =
                   |   if f(34) then "a"
                   |   else "b"
                   |  def test = i(f = _ == 34)
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Test.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)

      val fun = getMethod(clsNode, "test")
      val instructions = instructionsFromMethod(fun)
      val expected =
        List(
          Ldc(LDC, "a"),
          Op(ARETURN)
        )

      assert(instructions == expected,
        "`i was not properly inlined in `test`\n" + diffInstructions(instructions, expected))

    }
  }

  @Test def beta_reduce_under_block = {
    val source = """class Test:
                   |  def test =
                   |    {
                   |      val a = 3
                   |      (i: Int) => i + a
                   |    }.apply(2)
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Test.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)

      val fun = getMethod(clsNode, "test")
      val instructions = instructionsFromMethod(fun)
      val expected =
        List(
          Op(ICONST_3),
          VarOp(ISTORE, 1),
          Op(ICONST_2),
          VarOp(ILOAD, 1),
          Op(IADD),
          Op(IRETURN),
        )

      assert(instructions == expected,
        "`i was not properly beta-reduced in `test`\n" + diffInstructions(instructions, expected))

    }
  }

  @Test def i9456 = {
    val source = """class Foo {
                   |  def test: Int = inline2(inline1(2.+))
                   |
                   |  inline def inline1(inline f: Int => Int): Int => Int = i => f(1)
                   |
                   |  inline def inline2(inline f: Int => Int): Int = f(2) + 3
                   |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)

      val fun = getMethod(clsNode, "test")
      val instructions = instructionsFromMethod(fun)
      val expected = // TODO room for constant folding
        List(
          Op(ICONST_1),
          VarOp(ISTORE, 1),
          Op(ICONST_2),
          VarOp(ILOAD, 1),
          Op(IADD),
          Op(ICONST_3),
          Op(IADD),
          Op(IRETURN),
        )
      assert(instructions == expected,
        "`f` was not properly inlined in `fun`\n" + diffInstructions(instructions, expected))

    }
  }

  @Test def any_eq_specialization = {
    val source = """class Test:
                   |  inline def eql(x: Any, y: Any) = x == y
                   |
                   |  def testAny(x: Any, y: Any) = eql(x, y)
                   |  def testAnyExpected(x: Any, y: Any) = x == y
                   |
                   |  def testBoolean(x: Boolean, y: Boolean) = eql(x, y)
                   |  def testBooleanExpected(x: Boolean, y: Boolean) = x == y
                   |
                   |  def testByte(x: Byte, y: Byte) = eql(x, y)
                   |  def testByteExpected(x: Byte, y: Byte) = x == y
                   |
                   |  def testShort(x: Short, y: Short) = eql(x, y)
                   |  def testShortExpected(x: Short, y: Short) = x == y
                   |
                   |  def testInt(x: Int, y: Int) = eql(x, y)
                   |  def testIntExpected(x: Int, y: Int) = x == y
                   |
                   |  def testLong(x: Long, y: Long) = eql(x, y)
                   |  def testLongExpected(x: Long, y: Long) = x == y
                   |
                   |  def testFloat(x: Float, y: Float) = eql(x, y)
                   |  def testFloatExpected(x: Float, y: Float) = x == y
                   |
                   |  def testDouble(x: Double, y: Double) = eql(x, y)
                   |  def testDoubleExpected(x: Double, y: Double) = x == y
                   |
                   |  def testChar(x: Char, y: Char) = eql(x, y)
                   |  def testCharExpected(x: Char, y: Char) = x == y
                   |
                   |  def testUnit(x: Unit, y: Unit) = eql(x, y)
                   |  def testUnitExpected(x: Unit, y: Unit) = x == y
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Test.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)

      for cls <- List("Boolean", "Byte", "Short", "Int", "Long", "Float", "Double", "Char", "Unit") do
        val meth1      = getMethod(clsNode, s"test$cls")
        val meth2      = getMethod(clsNode, s"test${cls}Expected")

        val instructions1 = instructionsFromMethod(meth1)
        val instructions2 = instructionsFromMethod(meth2)

        assert(instructions1 == instructions2,
          s"`==` was not properly specialized when inlined in `test$cls`\n" +
          diffInstructions(instructions1, instructions2))
    }
  }

  @Test def any_neq_specialization = {
    val source = """class Test:
                   |  inline def neql(x: Any, y: Any) = x != y
                   |
                   |  def testAny(x: Any, y: Any) = neql(x, y)
                   |  def testAnyExpected(x: Any, y: Any) = x != y
                   |
                   |  def testBoolean(x: Boolean, y: Boolean) = neql(x, y)
                   |  def testBooleanExpected(x: Boolean, y: Boolean) = x != y
                   |
                   |  def testByte(x: Byte, y: Byte) = neql(x, y)
                   |  def testByteExpected(x: Byte, y: Byte) = x != y
                   |
                   |  def testShort(x: Short, y: Short) = neql(x, y)
                   |  def testShortExpected(x: Short, y: Short) = x != y
                   |
                   |  def testInt(x: Int, y: Int) = neql(x, y)
                   |  def testIntExpected(x: Int, y: Int) = x != y
                   |
                   |  def testLong(x: Long, y: Long) = neql(x, y)
                   |  def testLongExpected(x: Long, y: Long) = x != y
                   |
                   |  def testFloat(x: Float, y: Float) = neql(x, y)
                   |  def testFloatExpected(x: Float, y: Float) = x != y
                   |
                   |  def testDouble(x: Double, y: Double) = neql(x, y)
                   |  def testDoubleExpected(x: Double, y: Double) = x != y
                   |
                   |  def testChar(x: Char, y: Char) = neql(x, y)
                   |  def testCharExpected(x: Char, y: Char) = x != y
                   |
                   |  def testUnit(x: Unit, y: Unit) = neql(x, y)
                   |  def testUnitExpected(x: Unit, y: Unit) = x != y
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Test.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)

      for cls <- List("Boolean", "Byte", "Short", "Int", "Long", "Float", "Double", "Char", "Unit") do
        val meth1      = getMethod(clsNode, s"test$cls")
        val meth2      = getMethod(clsNode, s"test${cls}Expected")

        val instructions1 = instructionsFromMethod(meth1)
        val instructions2 = instructionsFromMethod(meth2)

        assert(instructions1 == instructions2,
          s"`!=` was not properly specialized when inlined in `test$cls`\n" +
          diffInstructions(instructions1, instructions2))
    }
  }
}
