package dotty.tools.backend.jvm

import org.junit.Assert._
import org.junit.Test

import scala.tools.asm.Opcodes._

import scala.collection.JavaConverters._

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

  @Test def i4947 = {
    val source = """class Foo {
                   |  inline def track[T](f: => T) <: T = {
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
                   |  inline def track2[T](f: => T) <: T = {
                   |    foo("tracking2") // line 3
                   |    f // line 4
                   |  }
                   |  inline def track[T](f: => T) <: T = {
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
                   |  inline def track2[T](f: => T) <: T = {
                   |    foo("tracking2") // line 3
                   |    f // line 4
                   |  }
                   |  inline def track[T](f: => T) <: T = {
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
                   |  inline def track2[T](f: => T) <: T = {
                   |    foo("tracking2") // line 3
                   |    f // line 4
                   |  }
                   |  inline def track[T](f: => T) <: T = {
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
                   |    inline def f() = {
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
          IntOp(BIPUSH, 10)
          , VarOp(ISTORE, 1)
          , VarOp(ILOAD, 1)
          , Op(ICONST_1)
          , Op(IADD)
          , VarOp(ISTORE, 1)
          , VarOp(ILOAD, 1)
          , Op(IRETURN)
        )
      assert(instructions == expected,
        "`f` was not properly inlined in `fun`\n" + diffInstructions(instructions, expected))

    }
  }
}
