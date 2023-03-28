package dotty.tools.backend.jvm

import org.junit.Test
import org.junit.Assert._

import scala.tools.asm.Opcodes._

class ArrayApplyOptTest extends DottyBytecodeTest {
  import ASMConverters._

  @Test def testArrayEmptyGenericApply = {
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

    test("IArray[String]()", List(Op(ICONST_0), TypeOp(ANEWARRAY, "java/lang/String"), Op(POP), Op(RETURN)))
    test("IArray[Unit]()", List(Op(ICONST_0), TypeOp(ANEWARRAY, "scala/runtime/BoxedUnit"), Op(POP), Op(RETURN)))
    test("IArray[Object]()", List(Op(ICONST_0), TypeOp(ANEWARRAY, "java/lang/Object"), Op(POP), Op(RETURN)))
    test("IArray[Boolean]()", newArray0Opcodes(T_BOOLEAN))
    test("IArray[Byte]()", newArray0Opcodes(T_BYTE))
    test("IArray[Short]()", newArray0Opcodes(T_SHORT))
    test("IArray[Int]()", newArray0Opcodes(T_INT))
    test("IArray[Long]()", newArray0Opcodes(T_LONG))
    test("IArray[Float]()", newArray0Opcodes(T_FLOAT))
    test("IArray[Double]()", newArray0Opcodes(T_DOUBLE))
    test("IArray[Char]()", newArray0Opcodes(T_CHAR))
    test("IArray[T]()", newArray0Opcodes(T_INT))
  }

  @Test def testArrayGenericApply = {
    def opCodes(tpe: String) =
      List(Op(ICONST_2), TypeOp(ANEWARRAY, tpe), Op(DUP), Op(ICONST_0), Ldc(LDC, "a"), Op(AASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, "b"), Op(AASTORE), Op(POP), Op(RETURN))
    test("""Array("a", "b")""", opCodes("java/lang/String"))
    test("""Array[Object]("a", "b")""", opCodes("java/lang/Object"))

    test("""IArray("a", "b")""", opCodes("java/lang/String"))
    test("""IArray[Object]("a", "b")""", opCodes("java/lang/Object"))
  }

  @Test def testArrayApplyBoolean = {
    val init = List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(BASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_0), Op(BASTORE))
    test("Array(true, false)", newArray2Opcodes(T_BOOLEAN, init))
    test("IArray(true, false)", newArray2Opcodes(T_BOOLEAN, init))
  }

  @Test def testArrayApplyByte = {
    val init = List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(BASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(BASTORE))
    test("Array(1: Byte, 2: Byte)", newArray2Opcodes(T_BYTE, init))
    test("IArray(1: Byte, 2: Byte)", newArray2Opcodes(T_BYTE, init))
  }

  @Test def testArrayApplyShort = {
    val init = List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(SASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(SASTORE))
    test("Array(1: Short, 2: Short)", newArray2Opcodes(T_SHORT, init))
    test("IArray(1: Short, 2: Short)", newArray2Opcodes(T_SHORT, init))
  }

  @Test def testArrayApplyInt = {
    val init = List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(IASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(IASTORE))
    test("Array(1, 2)", newArray2Opcodes(T_INT, init))
    test("IArray(1, 2)", newArray2Opcodes(T_INT, init))

    val init2 = List(Op(DUP), Op(ICONST_0), Field(GETSTATIC, "Foo$", "MODULE$", "LFoo$;"), Invoke(INVOKEVIRTUAL, "Foo$", "t", "()I", false), Op(IASTORE), Op(DUP), Op(ICONST_1), Field(GETSTATIC, "Foo$", "MODULE$", "LFoo$;"), Invoke(INVOKEVIRTUAL, "Foo$", "t", "()I", false), Op(IASTORE), TypeOp(CHECKCAST, "[I"))
    test("""Array[T](t, t): Array[T]""", newArray2Opcodes(T_INT, init2))
    test("""IArray[T](t, t): IArray[T]""", newArray2Opcodes(T_INT, init2))
  }

  @Test def testArrayApplyLong = {
    val init = List(Op(DUP), Op(ICONST_0), Ldc(LDC, 2), Op(LASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, 3), Op(LASTORE))
    test("Array(2L, 3L)", newArray2Opcodes(T_LONG, init))
    test("IArray(2L, 3L)", newArray2Opcodes(T_LONG, init))
  }

  @Test def testArrayApplyFloat = {
    val init = List(Op(DUP), Op(ICONST_0), Ldc(LDC, 2.1f), Op(FASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, 3.1f), Op(FASTORE))
    test("Array(2.1f, 3.1f)", newArray2Opcodes(T_FLOAT, init))
    test("IArray(2.1f, 3.1f)", newArray2Opcodes(T_FLOAT, init))
  }

  @Test def testArrayApplyDouble = {
    val init = List(Op(DUP), Op(ICONST_0), Ldc(LDC, 2.2d), Op(DASTORE), Op(DUP), Op(ICONST_1), Ldc(LDC, 3.2d), Op(DASTORE))
    test("Array(2.2d, 3.2d)", newArray2Opcodes(T_DOUBLE, init))
    test("IArray(2.2d, 3.2d)", newArray2Opcodes(T_DOUBLE, init))
  }

  @Test def testArrayApplyChar = {
    val init = List(Op(DUP), Op(ICONST_0), IntOp(BIPUSH, 120), Op(CASTORE), Op(DUP), Op(ICONST_1), IntOp(BIPUSH, 121), Op(CASTORE))
    test("Array('x', 'y')", newArray2Opcodes(T_CHAR, init))
    test("IArray('x', 'y')", newArray2Opcodes(T_CHAR, init))
  }

  @Test def testArrayApplyUnit = {
    test("Array[Unit]((), ())", List(Op(ICONST_2), TypeOp(ANEWARRAY, "scala/runtime/BoxedUnit"), Op(DUP),
        Op(ICONST_0), Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"), Op(AASTORE), Op(DUP),
        Op(ICONST_1), Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"), Op(AASTORE), Op(POP), Op(RETURN)))
    test("IArray[Unit]((), ())", List(Op(ICONST_2), TypeOp(ANEWARRAY, "scala/runtime/BoxedUnit"), Op(DUP),
        Op(ICONST_0), Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"), Op(AASTORE), Op(DUP),
        Op(ICONST_1), Field(GETSTATIC, "scala/runtime/BoxedUnit", "UNIT", "Lscala/runtime/BoxedUnit;"), Op(AASTORE), Op(POP), Op(RETURN)))
  }

  @Test def testArrayInlined = test(
    """{
      |  inline def array(inline xs: Int*): Array[Int] = Array(xs*)
      |  array(1, 2)
      |}""".stripMargin,
    newArray2Opcodes(T_INT, List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(IASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(IASTORE), TypeOp(CHECKCAST, "[I")))
  )

  @Test def testArrayInlined2 = test(
    """{
      |  inline def array(inline x: Int, inline xs: Int*): Array[Int] = Array(x, xs*)
      |  array(1, 2)
      |}""".stripMargin,
    newArray2Opcodes(T_INT, List(Op(DUP), Op(ICONST_0), Op(ICONST_1), Op(IASTORE), Op(DUP), Op(ICONST_1), Op(ICONST_2), Op(IASTORE)))
  )

  @Test def testArrayInlined3 = test(
    """{
      |  inline def array[T](inline xs: T*)(using inline ct: scala.reflect.ClassTag[T]): Array[T] = Array(xs*)
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

  @Test def testListApplyAvoidsIntermediateArray = {
    val source =
      """
        |class Foo {
        |  def meth1: List[String] = List("1", "2", "3")
        |  def meth2: List[String] =
        | new scala.collection.immutable.::("1", new scala.collection.immutable.::("2", new scala.collection.immutable.::("3", scala.collection.immutable.Nil))).asInstanceOf[List[String]]
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Foo.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val meth1   = getMethod(clsNode, "meth1")
      val meth2   = getMethod(clsNode, "meth2")

      val instructions1 = instructionsFromMethod(meth1)
      val instructions2 = instructionsFromMethod(meth2)

      assert(instructions1 == instructions2,
        "the List.apply method " +
          diffInstructions(instructions1, instructions2))
    }
  }

}
