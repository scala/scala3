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

    test("IArray[String]()", List(Op(ICONST_0), TypeOp(ANEWARRAY, "java/lang/String"), TypeOp(CHECKCAST, "[Ljava/lang/String;"), Op(POP), Op(RETURN)))
    test("IArray[Unit]()", List(Op(ICONST_0), TypeOp(ANEWARRAY, "scala/runtime/BoxedUnit"), TypeOp(CHECKCAST, "[Lscala/runtime/BoxedUnit;"), Op(POP), Op(RETURN)))
    test("IArray[Object]()", List(Op(ICONST_0), TypeOp(ANEWARRAY, "java/lang/Object"), TypeOp(CHECKCAST, "[Ljava/lang/Object;"), Op(POP), Op(RETURN)))
    test("IArray[Boolean]()", newArray0Opcodes(T_BOOLEAN, TypeOp(CHECKCAST, "[Z") :: Nil))
    test("IArray[Byte]()", newArray0Opcodes(T_BYTE, TypeOp(CHECKCAST, "[B") :: Nil))
    test("IArray[Short]()", newArray0Opcodes(T_SHORT, TypeOp(CHECKCAST, "[S") :: Nil))
    test("IArray[Int]()", newArray0Opcodes(T_INT, TypeOp(CHECKCAST, "[I") :: Nil))
    test("IArray[Long]()", newArray0Opcodes(T_LONG, TypeOp(CHECKCAST, "[J") :: Nil))
    test("IArray[Float]()", newArray0Opcodes(T_FLOAT, TypeOp(CHECKCAST, "[F") :: Nil))
    test("IArray[Double]()", newArray0Opcodes(T_DOUBLE, TypeOp(CHECKCAST, "[D") :: Nil))
    test("IArray[Char]()", newArray0Opcodes(T_CHAR, TypeOp(CHECKCAST, "[C") :: Nil))
    test("IArray[T]()", newArray0Opcodes(T_INT, TypeOp(CHECKCAST, "[I") :: Nil))
  }

  @Test def testArrayGenericApply= {
    def opCodes(tpe: String) =
      List(Op(ICONST_2), TypeOp(ANEWARRAY, tpe), Op(DUP), Op(ICONST_0), Ldc(LDC, "a"), Op(AASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, "b"), Op(AASTORE), Op(POP), Op(RETURN))
    test("""Array("a", "b")""", opCodes("java/lang/String"))
    test("""Array[Object]("a", "b")""", opCodes("java/lang/Object"))

    def opCodes2(tpe: String) =
      List(Op(ICONST_2), TypeOp(ANEWARRAY, tpe), Op(DUP), Op(ICONST_0), Ldc(LDC, "a"), Op(AASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, "b"), Op(AASTORE), TypeOp(CHECKCAST, s"[L$tpe;"), Op(POP), Op(RETURN))
    test("""IArray("a", "b")""", opCodes2("java/lang/String"))
    test("""IArray[Object]("a", "b")""", opCodes2("java/lang/Object"))
  }

  @Test def testArrayApplyBoolean = {
    val init = List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(BASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_0), Op(BASTORE))
    test("Array(true, false)", newArray2Opcodes(T_BOOLEAN, init))
    test("IArray(true, false)", newArray2Opcodes(T_BOOLEAN, init :+ TypeOp(CHECKCAST, "[Z")))
  }

  @Test def testArrayApplyByte = {
    val init = List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(BASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(BASTORE))
    test("Array[Byte](1, 2)", newArray2Opcodes(T_BYTE, init))
    test("IArray[Byte](1, 2)", newArray2Opcodes(T_BYTE, init :+ TypeOp(CHECKCAST, "[B")))
  }

  @Test def testArrayApplyShort = {
    val init = List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(SASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(SASTORE))
    test("Array[Short](1, 2)", newArray2Opcodes(T_SHORT, init))
    test("IArray[Short](1, 2)", newArray2Opcodes(T_SHORT, init :+ TypeOp(CHECKCAST, "[S")))
  }

  @Test def testArrayApplyInt = {
    val init = List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(IASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(IASTORE))
    test("Array(1, 2)", newArray2Opcodes(T_INT, init))
    test("IArray(1, 2)", newArray2Opcodes(T_INT, init :+ TypeOp(CHECKCAST, "[I")))

    val init2 = List(Op(DUP), Op(ICONST_0), Field(GETSTATIC, "Foo$", "MODULE$", "LFoo$;"), Invoke(INVOKEVIRTUAL, "Foo$", "t", "()I", false), Op(IASTORE), Op(DUP), Op(ICONST_1), Field(GETSTATIC, "Foo$", "MODULE$", "LFoo$;"), Invoke(INVOKEVIRTUAL, "Foo$", "t", "()I", false), Op(IASTORE))
    test("""Array[T](t, t)""", newArray2Opcodes(T_INT, init2))
    test("""IArray[T](t, t)""", newArray2Opcodes(T_INT, init2 :+ TypeOp(CHECKCAST, "[I")))
  }

  @Test def testArrayApplyLong = {
    val init = List(Op(DUP), Op(ICONST_0), Ldc(LDC, 2), Op(LASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, 3), Op(LASTORE))
    test("Array(2L, 3L)", newArray2Opcodes(T_LONG, init))
    test("IArray(2L, 3L)", newArray2Opcodes(T_LONG, init :+ TypeOp(CHECKCAST, "[J")))
  }

  @Test def testArrayApplyFloat = {
    val init = List(Op(DUP), Op(ICONST_0), Ldc(LDC, 2.1f), Op(FASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, 3.1f), Op(FASTORE))
    test("Array(2.1f, 3.1f)", newArray2Opcodes(T_FLOAT, init))
    test("IArray(2.1f, 3.1f)", newArray2Opcodes(T_FLOAT, init :+ TypeOp(CHECKCAST, "[F")))
  }

  @Test def testArrayApplyDouble = {
    val init = List(Op(DUP), Op(ICONST_0), Ldc(LDC, 2.2d), Op(DASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, 3.2d), Op(DASTORE))
    test("Array(2.2d, 3.2d)", newArray2Opcodes(T_DOUBLE, init))
    test("IArray(2.2d, 3.2d)", newArray2Opcodes(T_DOUBLE, init :+ TypeOp(CHECKCAST, "[D")))
  }

  @Test def testArrayApplyChar = {
    val init = List(Op(DUP), Op(ICONST_0), IntOp(BIPUSH, 120), Op(CASTORE), Op(DUP), Op(ICONST_1), IntOp(BIPUSH, 121), Op(CASTORE))
    test("Array('x', 'y')", newArray2Opcodes(T_CHAR, init))
    test("IArray('x', 'y')", newArray2Opcodes(T_CHAR, init :+ TypeOp(CHECKCAST, "[C")))
  }

  @Test def testArrayApplyUnit = {
    test("Array[Unit]((), ())", List(Op(ICONST_2), TypeOp(ANEWARRAY, "scala/runtime/BoxedUnit"), Op(DUP),
        Op(ICONST_0), Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"), Op(AASTORE), Op(DUP),
        Op(ICONST_1), Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"), Op(AASTORE), Op(POP), Op(RETURN)))
    test("IArray[Unit]((), ())", List(Op(ICONST_2), TypeOp(ANEWARRAY, "scala/runtime/BoxedUnit"), Op(DUP),
        Op(ICONST_0), Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"), Op(AASTORE), Op(DUP),
        Op(ICONST_1), Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"), Op(AASTORE), TypeOp(CHECKCAST, "[Lscala/runtime/BoxedUnit;"), Op(POP), Op(RETURN)))
  }

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

  @Test def testArrayInlined3 = test(
    """{
      |  inline def array[T](xs: =>T*)(given ct: =>scala.reflect.ClassTag[T]): Array[T] = Array(xs: _*)
      |  array(1, 2)
      |}""".stripMargin,
    newArray2Opcodes(T_INT, List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(IASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(IASTORE), TypeOp(CHECKCAST, "[I")))
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
