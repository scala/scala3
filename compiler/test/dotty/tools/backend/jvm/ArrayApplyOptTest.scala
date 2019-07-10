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
    test("Array[Boolean]()", newArray0Opcodes(T_BOOLEAN))
    test("Array[Byte]()", newArray0Opcodes(T_BYTE))
    test("Array[Short]()", newArray0Opcodes(T_SHORT))
    test("Array[Int]()", newArray0Opcodes(T_INT))
    test("Array[Long]()", newArray0Opcodes(T_LONG))
    test("Array[Float]()", newArray0Opcodes(T_FLOAT))
    test("Array[Double]()", newArray0Opcodes(T_DOUBLE))
    test("Array[Char]()", newArray0Opcodes(T_CHAR))
    test("Array[T]()", newArray0Opcodes(T_INT))
  }

  @Test def testArrayGenericApply= {
    def opCodes(tpe: String) =
      List(Op(ICONST_2), TypeOp(ANEWARRAY, tpe), Op(DUP), Op(ICONST_0), Ldc(LDC, "a"), Op(AASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, "b"), Op(AASTORE), Op(POP), Op(RETURN))
    test("""Array("a", "b")""", opCodes("java/lang/String"))
    test("""Array[Object]("a", "b")""", opCodes("java/lang/Object"))
  }

  @Test def testArrayApplyBoolean =
    test("Array(true, false)", newArray2Opcodes(T_BOOLEAN, List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(BASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_0), Op(BASTORE))))

  @Test def testArrayApplyByte =
    test("Array[Byte](1, 2)", newArray2Opcodes(T_BYTE, List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(BASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(BASTORE))))

  @Test def testArrayApplyShort =
    test("Array[Short](1, 2)", newArray2Opcodes(T_SHORT, List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(SASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(SASTORE))))

  @Test def testArrayApplyInt = {
    test("Array(1, 2)", newArray2Opcodes(T_INT, List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(IASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(IASTORE))))
    test("""Array[T](t, t)""", newArray2Opcodes(T_INT, List(Op(DUP), Op(ICONST_0), Field(GETSTATIC, "Foo$", "MODULE$", "LFoo$;"), Invoke(INVOKEVIRTUAL, "Foo$", "t", "()I", false), Op(IASTORE), Op(DUP), Op(ICONST_1), Field(GETSTATIC, "Foo$", "MODULE$", "LFoo$;"), Invoke(INVOKEVIRTUAL, "Foo$", "t", "()I", false), Op(IASTORE))))
  }

  @Test def testArrayApplyLong =
    test("Array(2L, 3L)", newArray2Opcodes(T_LONG, List(Op(DUP), Op(ICONST_0), Ldc(LDC, 2), Op(LASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, 3), Op(LASTORE))))

  @Test def testArrayApplyFloat =
    test("Array(2.1f, 3.1f)", newArray2Opcodes(T_FLOAT, List(Op(DUP), Op(ICONST_0), Ldc(LDC, 2.1f), Op(FASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, 3.1f), Op(FASTORE))))

  @Test def testArrayApplyDouble =
    test("Array(2.2d, 3.2d)", newArray2Opcodes(T_DOUBLE, List(Op(DUP), Op(ICONST_0), Ldc(LDC, 2.2d), Op(DASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, 3.2d), Op(DASTORE))))

  @Test def testArrayApplyChar =
    test("Array('x', 'y')", newArray2Opcodes(T_CHAR, List(Op(DUP), Op(ICONST_0), IntOp(BIPUSH, 120), Op(CASTORE), Op(DUP), Op(ICONST_1), IntOp(BIPUSH, 121), Op(CASTORE))))

  @Test def testArrayApplyUnit =
    test("Array[Unit]((), ())", List(Op(ICONST_2), TypeOp(ANEWARRAY, "scala/runtime/BoxedUnit"), Op(DUP),
        Op(ICONST_0), Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"), Op(AASTORE), Op(DUP),
        Op(ICONST_1), Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"), Op(AASTORE), Op(POP), Op(RETURN)))

  @Test def testArrayInlined = test(
    """{
      |  inline def array(xs: =>Int*): Array[Int] = Array(xs: _*)
      |  array(1, 2)
      |}""".stripMargin,
    newArray2Opcodes(T_INT, List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(IASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(IASTORE), TypeOp(CHECKCAST, "[I")))
  )

  @Test def testArrayInlined2 = test(
    """{
      |  inline def array(x: =>Int, xs: =>Int*): Array[Int] = Array(x, xs: _*)
      |  array(1, 2)
      |}""".stripMargin,
    newArray2Opcodes(T_INT, List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(IASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(IASTORE)))
  )

  private def newArray0Opcodes(tpe: Int, init: List[Any] = Nil): List[Any] =
    Op(ICONST_0) :: IntOp(NEWARRAY, tpe) :: init ::: Op(POP) :: Op(RETURN) :: Nil

  private def newArray2Opcodes(tpe: Int, init: List[Any] = Nil): List[Any] =
    Op(ICONST_2) :: IntOp(NEWARRAY, tpe) :: init ::: Op(POP) :: Op(RETURN) :: Nil

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
