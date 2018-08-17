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
                 |  transparent def foo: Int = 1
                 |  @forceInline def bar: Int = 1
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
                   |  transparent def track[T](f: => T): T = {
                   |    println("tracking") // line 3
                   |    f // line 4
                   |  }
                   |  def main(args: Array[String]): Unit = { // line 6
                   |    track { // line 7
                   |      println("abc") // line 8
                   |      track { // line 9
                   |        println("inner") // line 10
                   |      }
                   |    } // line 11
                   |  }
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
          LineNumber(6, Label(0)), // Position of the method start
          LineNumber(7, Label(0)), // Position of the call to `track`
          Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
          Ldc(LDC, "tracking"),
          Invoke(INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
          Label(6),
          LineNumber(8, Label(6)), // Actual position
          Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
          Ldc(LDC, "abc"),
          Invoke(INVOKEVIRTUAL, "scala/Predef$", "println","(Ljava/lang/Object;)V", false),
          Label(11),
          LineNumber(9, Label(11)), // Position of the call to `track`
          Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
          Ldc(LDC, "tracking"),
          Invoke(INVOKEVIRTUAL, "scala/Predef$", "println","(Ljava/lang/Object;)V", false),
          Label(16),
          LineNumber(10, Label(16)), // Actual position
          Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
          Ldc(LDC, "inner"),
          Invoke(INVOKEVIRTUAL, "scala/Predef$", "println","(Ljava/lang/Object;)V", false),
          Op(RETURN),
          Label(22)
        )
        assert(instructions == expected,
          "`track` was not properly inlined in `main`\n" + diffInstructions(instructions, expected))

    }
  }

  @Test def i4947b = {
    val source = """class Foo {
                   |  transparent def track2[T](f: => T): T = {
                   |    println("tracking2") // line 3
                   |    f // line 4
                   |  }
                   |  transparent def track[T](f: => T): T = {
                   |    println("tracking") // line 7
                   |    track2 { // line 8
                   |      f // line 9
                   |    }
                   |  }
                   |  def main(args: Array[String]): Unit = { // line 12
                   |    track { // line 13
                   |      println("abc") // line 14
                   |    }
                   |  }
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
          LineNumber(12, Label(0)), // Position of the method start
          LineNumber(13, Label(0)), // Position of the call to `track`
          Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
          Ldc(LDC, "tracking"),
          Invoke(INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
          Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
          Ldc(LDC, "tracking2"),
          Invoke(INVOKEVIRTUAL, "scala/Predef$", "println","(Ljava/lang/Object;)V", false),
          Label(9),
          LineNumber(14, Label(9)), // Actual position
          Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
          Ldc(LDC, "abc"),
          Invoke(INVOKEVIRTUAL, "scala/Predef$", "println","(Ljava/lang/Object;)V", false),
          Op(RETURN),
          Label(15)
        )
        assert(instructions == expected,
          "`track` was not properly inlined in `main`\n" + diffInstructions(instructions, expected))

    }
  }

  @Test def i4947c = {
    val source = """class Foo {
                   |  transparent def track2[T](f: => T): T = {
                   |    println("tracking2") // line 3
                   |    f // line 4
                   |  }
                   |  transparent def track[T](f: => T): T = {
                   |    track2 { // line 7
                   |      println("fgh") // line 8
                   |      f // line 9
                   |    }
                   |  }
                   |  def main(args: Array[String]): Unit = { // line 12
                   |    track { // line 13
                   |      println("abc") // line 14
                   |    }
                   |  }
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
          LineNumber(12, Label(0)), // Position of the method start
          LineNumber(13, Label(0)), // Position of the call to `track`
          Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
          Ldc(LDC, "tracking2"),
          Invoke(INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
          Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
          Ldc(LDC, "fgh"),
          Invoke(INVOKEVIRTUAL, "scala/Predef$", "println","(Ljava/lang/Object;)V", false),
          Label(9),
          LineNumber(14, Label(9)), // Actual position
          Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
          Ldc(LDC, "abc"),
          Invoke(INVOKEVIRTUAL, "scala/Predef$", "println","(Ljava/lang/Object;)V", false),
          Op(RETURN),
          Label(15)
        )
        assert(instructions == expected,
          "`track` was not properly inlined in `main`\n" + diffInstructions(instructions, expected))

    }
  }

  @Test def i4947d = {
    val source = """class Foo {
                   |  transparent def track2[T](f: => T): T = {
                   |    println("tracking2") // line 3
                   |    f // line 4
                   |  }
                   |  transparent def track[T](f: => T): T = {
                   |    track2 { // line 7
                   |      track2 { // line 8
                   |        f // line 9
                   |      }
                   |    }
                   |  }
                   |  def main(args: Array[String]): Unit = { // line 13
                   |    track { // line 14
                   |      println("abc") // line 15
                   |    }
                   |  }
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
          LineNumber(13, Label(0)), // Position of the method start
          LineNumber(14, Label(0)), // Position of the call to `track`
          Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
          Ldc(LDC, "tracking2"),
          Invoke(INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
          Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
          Ldc(LDC, "tracking2"),
          Invoke(INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
          Label(9),
          LineNumber(15, Label(9)), // Actual position
          Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
          Ldc(LDC, "abc"),
          Invoke(INVOKEVIRTUAL, "scala/Predef$", "println","(Ljava/lang/Object;)V", false),
          Op(RETURN),
          Label(15)
        )
        assert(instructions == expected,
          "`track` was not properly inlined in `main`\n" + diffInstructions(instructions, expected))

    }
  }
}
