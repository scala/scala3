package dotty.tools.backend.jvm

import org.junit.Test
import org.junit.Assert._

import scala.tools.asm.Opcodes._

class ArrayApplyOptTest extends DottyBytecodeTest {
  import ASMConverters._

  @Test def testArrayEmptyGenericApply= {
    test("Array[String]()", List(Op(ICONST_0), TypeOp(ANEWARRAY, "java/lang/String"), Op(POP), Op(RETURN)))
    test("Array[Unit]()", List(Op(ICONST_0), TypeOp(ANEWARRAY, "scala/runtime/BoxedUnit"), Op(POP), Op(RETURN)))
    test("Array[Object]()", List(Op(ICONST_0), TypeOp(ANEWARRAY, "java/lang/Object"), Op(POP), Op(RETURN)))
    test("Array[Boolean]()", List(Op(ICONST_0), IntOp(NEWARRAY, 4), Op(POP), Op(RETURN)))
    test("Array[Char]()", List(Op(ICONST_0), IntOp(NEWARRAY, 5), Op(POP), Op(RETURN)))
    test("Array[Float]()", List(Op(ICONST_0), IntOp(NEWARRAY, 6), Op(POP), Op(RETURN)))
    test("Array[Double]()", List(Op(ICONST_0), IntOp(NEWARRAY, 7), Op(POP), Op(RETURN)))
    test("Array[Byte]()", List(Op(ICONST_0), IntOp(NEWARRAY, 8), Op(POP), Op(RETURN)))
    test("Array[Short]()", List(Op(ICONST_0), IntOp(NEWARRAY, 9), Op(POP), Op(RETURN)))
    test("Array[Int]()", List(Op(ICONST_0), IntOp(NEWARRAY, 10), Op(POP), Op(RETURN)))
    test("Array[Long]()", List(Op(ICONST_0), IntOp(NEWARRAY, 11), Op(POP), Op(RETURN)))
    test("Array[T]()", List(Op(ICONST_0), IntOp(NEWARRAY, 10), Op(POP), Op(RETURN)))
  }

  @Test def testArrayGenericApply= {
    test("""Array("a", "b")""", List(Op(ICONST_2), TypeOp(ANEWARRAY, "java/lang/String"), Op(DUP), Op(ICONST_0), Ldc(LDC, "a"), Op(AASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, "b"), Op(AASTORE), Op(POP), Op(RETURN)))
    test("""Array[Object]("a", "b")""", List(Op(ICONST_2), TypeOp(ANEWARRAY, "java/lang/Object"), Op(DUP), Op(ICONST_0), Ldc(LDC, "a"), Op(AASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, "b"), Op(AASTORE), Op(POP), Op(RETURN)))
  }

  @Test def testArrayApplyBoolean =
    test("Array(true, false)", List(Op(ICONST_2), IntOp(NEWARRAY, 4), Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(BASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_0), Op(BASTORE), Op(POP), Op(RETURN)))

  @Test def testArrayApplyByte =
    test("Array[Byte](1, 2)", List(Op(ICONST_2), IntOp(NEWARRAY, 8), Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(BASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(BASTORE), Op(POP), Op(RETURN)))

  @Test def testArrayApplyShort =
    test("Array[Short](1, 2)", List(Op(ICONST_2), IntOp(NEWARRAY, 9), Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(SASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(SASTORE), Op(POP), Op(RETURN)))

  @Test def testArrayApplyInt = {
    test("Array(1, 2)", List(Op(ICONST_2), IntOp(NEWARRAY, 10), Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(IASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(IASTORE), Op(POP), Op(RETURN)))
    test("""Array[T](t, t)""", List(Op(ICONST_2), IntOp(NEWARRAY, 10), Op(DUP), Op(ICONST_0), Field(GETSTATIC, "Foo$", "MODULE$", "LFoo$;"), Invoke(INVOKEVIRTUAL, "Foo$", "t", "()I", false), Op(IASTORE), Op(DUP), Op(ICONST_1), Field(GETSTATIC, "Foo$", "MODULE$", "LFoo$;"), Invoke(INVOKEVIRTUAL, "Foo$", "t", "()I", false), Op(IASTORE), Op(POP), Op(RETURN)))
  }

  @Test def testArrayApplyLong =
    test("Array(2L, 3L)", List(Op(ICONST_2), IntOp(NEWARRAY, 11), Op(DUP), Op(ICONST_0), Ldc(LDC, 2), Op(LASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, 3), Op(LASTORE), Op(POP), Op(RETURN)))

  @Test def testArrayApplyFloat =
    test("Array(2.1f, 3.1f)", List(Op(ICONST_2), IntOp(NEWARRAY, 6), Op(DUP), Op(ICONST_0), Ldc(LDC, 2.1f), Op(FASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, 3.1f), Op(FASTORE), Op(POP), Op(RETURN)))

  @Test def testArrayApplyDouble =
    test("Array(2.2d, 3.2d)", List(Op(ICONST_2), IntOp(NEWARRAY, 7), Op(DUP), Op(ICONST_0), Ldc(LDC, 2.2d), Op(DASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, 3.2d), Op(DASTORE), Op(POP), Op(RETURN)))

  @Test def testArrayApplyChar =
    test("Array('x', 'y')", List(Op(ICONST_2), IntOp(NEWARRAY, 5), Op(DUP), Op(ICONST_0), IntOp(BIPUSH, 120), Op(CASTORE), Op(DUP), Op(ICONST_1), IntOp(BIPUSH, 121), Op(CASTORE), Op(POP), Op(RETURN)))

  @Test def testArrayApplyUnit =
    test("Array[Unit]((), ())", List(Op(ICONST_2), TypeOp(ANEWARRAY, "scala/runtime/BoxedUnit"), Op(DUP),
        Op(ICONST_0), Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"), Op(AASTORE), Op(DUP),
        Op(ICONST_1), Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"), Op(AASTORE), Op(POP), Op(RETURN)))

  @Test def testArrayInlined = test(
    """{
      |  inline def array(xs: =>Int*): Array[Int] = Array(xs: _*)
      |  array(1, 2)
      |}""".stripMargin,
    List(Op(ICONST_2), IntOp(NEWARRAY, 10), Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(IASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(IASTORE), TypeOp(CHECKCAST, "[I"), Op(POP), Op(RETURN))
  )

  @Test def testArrayInlined2 = test(
    """{
      |  inline def array(x: =>Int, xs: =>Int*): Array[Int] = Array(x, xs: _*)
      |  array(1, 2)
      |}""".stripMargin,
    List(Op(ICONST_2), IntOp(NEWARRAY, 10), Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(IASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(IASTORE), Op(POP), Op(RETURN))
  )

  private def test(code: String, expectedInstructions: List[Any])= {
    val source =
      s"""class Foo {
         | import Foo._
         | def test: Unit = $code
         |}
         |object Foo {
         | opaque type T = Int
         | def t: T = 1
         |}
       """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Foo.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val meth   = getMethod(clsNode, "test")

      val instructions = instructionsFromMethod(meth)

      assertEquals(expectedInstructions, instructions)
    }
  }

}
