package dotty.tools.backend.jvm

import dotty.tools.backend.jvm.ASMConverters.instructionsFromMethod
import org.junit.{Ignore, Test}

class OptimizationBytecodeTests extends DottyBytecodeTest {
  def testEquivalence(expectedSource: String, actualSource: String, params: List[String] = Nil, extraMemberSources: List[String] = Nil, returnType: String = "Int"): Unit = {
    val source =
      f"""
        |class Test {
        |  ${extraMemberSources.mkString("\n")}
        |  def actual(${params.mkString(", ")}): $returnType = { $actualSource }
        |  def expected(${params.mkString(", ")}): $returnType = { $expectedSource }
        |}
         """.stripMargin

    checkBCode(source) { dir =>
      val clsIn = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val meth1 = getMethod(clsNode, "actual")
      val meth2 = getMethod(clsNode, "expected")

      val instructions1 = instructionsFromMethod(meth1)
      val instructions2 = instructionsFromMethod(meth2)

      assert(instructions1 == instructions2,
        "code was not properly optimized\n" +
          diffInstructions(instructions1, instructions2))
   }
  }


  @Test def inlineTuple2Specialized =
    testEquivalence(
      "1",
      "val t = (1, 2); t._1"
    )

  @Test def inlineTuple2SpecializedTwice =
    testEquivalence(
      "1",
      "val t = (1, 2); val t2 = (3, t._1); t2._2"
    )

  @Test def inlineTuple2OfTuple2Specialized =
    testEquivalence(
      "1 + 2 + 3",
      "val ((a, b), c) = ((1, 2), 3); a + b + c"
    )

  @Test def inlineTuple2SpecializedCastToGeneral =
    testEquivalence(
      "3",
      "val t = (3, 4); (t: Tuple2[Any, Any])._1.asInstanceOf[Int]"
    )

  @Test def inlineTuple3Specialized =
    testEquivalence(
      "1",
      "val t = (3, 1, 2); t._2"
    )

  @Test def inlineTuple2General =
    testEquivalence(
      "x",
      "val a: Any = x; val t = (a, 42); t._1.asInstanceOf[Int]",
      params = List("x: Int"),
    )

  @Test def inlineTuple2OfTuple2General =
    testEquivalence(
      "1 + 2 + 3",
      "val ((a, b), c) = ((1, 2), Integer.valueOf(3)); a + b + c"
    )

  @Test def inlineTuple2GeneralAndSpecialized =
    testEquivalence(
      "x",
      "val t1 = (x, 0); val a: Any = t1._1; val t2 = (a, 42); t2._1.asInstanceOf[Int]",
      params = List("x: Int")
    )


  @Test def inlineNonGenericIdentityFunction =
    testEquivalence(
      "1",
      "id(1)",
      extraMemberSources = List("final def id(x: Int): Int = x")
    )

  @Test def inlineGenericIdentityFunction =
    testEquivalence(
      "1",
      "id(1)",
      extraMemberSources = List("final def id[T](x: T): T = x")
    )


  @Test def inlineUnusedAdditionButKeepNullCheck =
    testEquivalence(
      "x.nn; 0",
      "add(x.nn, 0); 0",
      extraMemberSources = List("def add(x: Int, y: Int): Int = x + y"),
      params = List("x: Int | Null")
    )


  @Test def inlineBoxUnbox =
    testEquivalence(
      "x",
      "val a: Any = x; a.asInstanceOf[Int]",
      params = List("x: Int")
    )

  @Test def inlineBoxUnboxMixedArithmetic =
    testEquivalence(
      "x + y",
      "val a: java.lang.Integer = x; val b: java.lang.Long = y; a + b",
      params = List("x: Int", "y: Long"),
      returnType = "Long"
    )

  @Test def inlineBoxUnboxValEscaped =
    testEquivalence(
      "escape(x); x",
      "val a: Any = x; escape(a); a.asInstanceOf[Int]",
      params = List("x: Int"),
      extraMemberSources = List("@noinline def escape(a: Any): Unit = ???")
    )

  @Test def inlineBoxUnboxValEscapedBranch =
    testEquivalence(
      "escape(if b then 10 else 32); if b then 10 else 32",
      "val i = Integer.valueOf(10); val j = Integer.valueOf(32); escape(if b then i else j); if b then i.toInt else j.toInt",
      params = List("b: Boolean"),
      extraMemberSources = List("@noinline def escape(a: Any): Unit = ???")
    )

  @Test def inlineBoxUnboxVarClosure =
    testEquivalence(
      "x",
      "var a: Any = x; def c() = a.asInstanceOf[Int]; c()",
      params = List("x: Int")
    )

  @Test def inlineBoxUnboxBranch =
    testEquivalence(
      "if x >= 0 then 1 else 2",
      "var a = if x >= 0 then (1: Any) else 2; a.asInstanceOf[Int]",
      params = List("x: Int")
    )

  @Test def inlineBoxUnboxBranchContrived =
    testEquivalence(
      "String.valueOf(x)",
      "val wat: Any = if (x > 0) x else -x; wat match { case x: Int => String.valueOf(x); case _ => \"?\" }",
      params = List("x: Int"),
      returnType = "String"
    )

  @Test def inlineBoxUnboxJava =
    testEquivalence(
      "x",
      "val a = java.lang.Integer.valueOf(x); a.asInstanceOf[Int]",
      params = List("x: Int")
    )

  @Test def inlineBoxUnboxJavaBranch =
    testEquivalence(
      "10",
      "val b = java.lang.Boolean.valueOf(true); val a = if b then java.lang.Integer.valueOf(10) else 0; a.asInstanceOf[Int]"
    )

  @Test def inlineUnboxNull =
    testEquivalence(
      "0",
      "scala.runtime.BoxesRunTime.unboxToInt(null)"
    )

  @Test def inlineUnboxNull2 =
    testEquivalence(
      "0",
      "null.asInstanceOf[Int]"
    )


  @Test def inlineRef =
    testEquivalence(
      "0",
      "val r = scala.runtime.IntRef(0); r.elem"
    )

  @Test def inlineRefBranch =
    testEquivalence(
      "if b then 0 else 1",
      "val r1 = scala.runtime.IntRef.zero(); val r2 = scala.runtime.IntRef.create(1); val r = if b then r1 else r2; r.elem",
      params = List("b: Boolean")
    )

  @Test def inlineRefWrite =
    testEquivalence(
      "var l = 10L; l += 3; l",
      "val r = scala.runtime.LongRef.create(10); r.elem += 3; r.elem",
      returnType = "Long"
    )

  @Test def inlineRefUsedAsBranch =
    testEquivalence(
      "var x = false; if b then x = true; if b then 1 else 2",
      "val x = scala.runtime.BooleanRef.create(false); if b then x.elem = true; if x.elem then 1 else 2",
      params = List("b: Boolean")
    )


  @Test def inlineMatch =
    testEquivalence(
      "a + b",
      "(a, b) match { case (a: Int, b: Int) => a + b; case _ => -1 }",
      params = List("a: Int", "b: Int")
    )

  @Test def inlineMatchBoxed =
    testEquivalence(
      "a + b",
      "(a: Any, b: Any) match { case (a: Int, b: Int) => a + b; case _ => -1 }",
      params = List("a: Int", "b: Int")
    )

  @Test def inlineMatchResult =
    testEquivalence(
      "if n == 0 then 1 + 2 else 3 + 4",
      "val (a, b) = n match { case 0 => (1, 2); case _ => (3, 4) }; a + b",
      params = List("n: Int")
    )


  @Test def inlineClosure =
    testEquivalence(
      "42",
      "val x = 10; def c(y: Int): Int = x + y; c(32)"
    )


  @Test def aliasResolution =
    testEquivalence(
      "val x = foo(); val y = x.length; y + y",
      "val x = foo(); val y = x.length; y + x.length",
      extraMemberSources = List("def foo(): Array[Int] = ???")
    )


  @Test def loadAfterStore =
    testEquivalence(
      "1",
      "val x = 1; x"
    )

  @Test def loadAfterStoreNull =
    testEquivalence(
      "null",
      "val x: Int | Null = null; x",
      returnType = "Int | Null"
    )

  @Test def loadAfterStoreNullField =
    testEquivalence(
      "null",
      "x = null; if (x != null) x = 0; x",
      extraMemberSources = List("private var x : Int | Null = 0"),
      returnType = "Int | Null"
    )

  @Test def loadAfterStoreTwice =
    testEquivalence(
      "1",
      "val x = 1; val y = x; y"
    )


  @Test def deadStoreVal =
    testEquivalence(
      "1",
      "val x = 42; 1"
    )

  @Test def deadStoreVar =
    testEquivalence(
      "1",
      "var x = 42; 1"
    )

  @Test def deadStoreDueToSubsequentWriteLocal =
    testEquivalence(
      "1",
      "var x = 42; x = 1; x"
    )

  @Test def deadStoreDueToSubsequentWriteField =
    testEquivalence(
      "x = 1; 1",
      "x = 42; x = 1; x",
      extraMemberSources = List("private var x: Int = 0")
    )

  @Test def deadStoreDespiteRead =
    testEquivalence(
      "1",
      "var x = 42; val y = x; 1"
    )

  @Test def deadStoreDespiteCall =
    testEquivalence(
      "1",
      "def eat(x: Int): Unit = (); var x = 42; eat(x); 1"
    )

  @Test def deadStoreVarTryCatch =
    testEquivalence(
      "1",
      "var x = 42; try x = 1 catch case _ => (); x"
    )


  @Test def elidedCastPrimitive =
    testEquivalence(
      "x",
      "x.asInstanceOf[Int]",
      params = List("x: Int")
    )

  @Test def elidedCastClass =
    testEquivalence(
      "x",
      "x.asInstanceOf[String]",
      params = List("x: String"),
      returnType = "String"
    )

  @Test def elidedIsInstanceTop =
    testEquivalence(
      "true",
      "val b = (x: Integer); b.isInstanceOf[Object]",
      params = List("x: Int"),
      returnType = "Boolean"
    )

  @Test def elidedIsInstanceInterface =
    testEquivalence(
      "true",
      "val b = (x: Integer); b.isInstanceOf[Number]",
      params = List("x: Int"),
      returnType = "Boolean"
    )


  @Test def deadBoxJava =
    testEquivalence(
      "x",
      "java.lang.Integer.valueOf(x); x",
      params = List("x: Int")
    )

  @Test def deadBoxScala =
    testEquivalence(
      "x",
      "scala.runtime.BoxesRunTime.boxToInteger(x); x",
      params = List("x: Int")
    )


  @Test def deadCodeAfterReturn =
    testEquivalence(
      "1",
      "return 1; 2"
    )

  @Test def deadCodeAfterThrow =
    testEquivalence(
      "throw new Error()",
      "throw new Error(); 2"
    )


  @Test def deadCodeFromNullCheckOfConstantNull =
    testEquivalence(
      "2",
      "val x: String | Null = null; if (x == null) then return 2; 1"
    )

  @Test def deadCodeFromNonNullCheckOfConstantNull =
    testEquivalence(
      "1",
      "val x: String | Null = null; if (x != null) then return 2; 1"
    )

  @Test def deadCodeFromEqNullCheckOfConstantNull =
    testEquivalence(
      "2",
      "val x: String | Null = null; val y: String | Null = null; if (x == y) then return 2; 1"
    )

  @Test def deadCodeFromNeqNullCheckOfConstantNull =
    testEquivalence(
      "1",
      "val x: String | Null = null; val y: String | Null = null; if (x != y) then return 2; 1"
    )


  @Test def deadCodeFromNullCheckOfConstantString =
    testEquivalence(
      "1",
      "val x: String | Null = \"\"; if (x == null) then return 2; 1"
    )

  @Test def deadCodeFromNonNullCheckOfConstantString =
    testEquivalence(
      "2",
      "val x: String | Null = \"\"; if (x != null) then return 2; 1"
    )

  @Test def deadCodeFromEqNullCheckOfConstantString =
    testEquivalence(
      "1",
      "val x: String | Null = \"\"; val y: String | Null = null; if (x == y) then return 2; 1"
    )

  @Test def deadCodeFromNeqNullCheckOfConstantString =
    testEquivalence(
      "2",
      "val x: String | Null = \"\"; val y: String | Null = null; if (x != y) then return 2; 1"
    )


  @Test def deadCodeFromIsInstanceExact =
    testEquivalence(
      "1",
      "if x.isInstanceOf[String] then 1 else 2",
      params = List("x: String")
    )

  @Test def deadCodeFromIsInstanceAny =
    testEquivalence(
      "1",
      "if x.isInstanceOf[Any] then 1 else 2",
      params = List("x: String")
    )

  @Test def deadCodeFromIsInstanceNull =
    testEquivalence(
      "2",
      "x = null; if x.isInstanceOf[String] then 1 else 2",
      extraMemberSources = List("private var x: String = \"\"")
    )

  @Test def deadCodeFromIsInstanceUnrelated =
    testEquivalence(
      "2",
      "if x.isInstanceOf[List[Int]] then 1 else 2",
      params = List("x: String")
    )


  // REVIEW: these seem more questionable, what value do they add?
  @Test def classOfThenNewArray =
    testEquivalence(
      // REVIEW: observation while debugging --
      // the generated code for "def foo(): Array[Int] = Array[Int]()"
      // is "ICONST_0 ; NEWARRAY I; CHECKCAST [I; ARETURN",
      // surely the checkcast isn't needed?
      "Array[Int]()",
      "scala.reflect.ClassTag(classOf[Int]).newArray(0)",
      returnType = "Array[Int]"
    )
  @Test def classOfThenNewArrayThenArrayApply =
    testEquivalence(
      "Array[Int](5)(0)",
      "scala.runtime.ScalaRunTime.array_apply(scala.reflect.ClassTag(classOf[Int]).newArray(5), 0)",
      returnType = "Any"
    )
  @Test def classOfThenNewArrayThenArrayUpdate =
    testEquivalence(
      "Array[Int](5)(0) = 123",
      "scala.runtime.ScalaRunTime.array_update(scala.reflect.ClassTag(classOf[Int]).newArray(5), 0, 123)",
      returnType = "Unit"
    )
  @Test def arrayGetLength =
    testEquivalence(
      "x.length",
      "java.lang.reflect.Array.getLength(x)",
      params = List("x: Array[Int]")
    )
  @Test def arrayGetClass =
    testEquivalence(
      "classOf[Array[Int]]",
      "val x = Array[Int](); x.getClass",
      returnType = "Class[?]"
    )

  // REVIEW: the scala2 test suite has some tests for stuff like `array.map(_ + 1)` ensuring it can be inlined to a while loop without boxing, do we want this?
  //         or does HotSpot do our job for us?

  // TODO: more generally we should port scala2 optimizer unit tests as some are regression tests,
  //       e.g., https://github.com/scala/scala/pull/10404
}
