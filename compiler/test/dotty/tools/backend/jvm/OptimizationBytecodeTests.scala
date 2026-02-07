package dotty.tools.backend.jvm

import dotty.tools.backend.jvm.ASMConverters.instructionsFromMethod
import org.junit.{Ignore, Test}

@Ignore("Disabled until we have ported the Scala2 optimizer")
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


  @Test def inlineNonGenericLocallyFunction =
    testEquivalence(
      "1",
      "locally { 1 }",
      extraMemberSources = List("@inline def locally(x: Int): Int = x") // simplified from the form below
    )

  @Test def inlineGenericLocallyFunction =
    testEquivalence(
      "1",
      "locally { 1 }",
      extraMemberSources = List("@inline def locally[T](@deprecatedName(\"x\") x: T): T = x") // copied from Predef.scala
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


  @Test def deadUnbox = testEquivalence(
    "1",
    "Byte.unbox(b); 1",
    params = List("b: java.lang.Byte")
  )

  @Test def deadUnboxBox = testEquivalence(
    "1",
    "Int.unbox(Integer.valueOf(n)); 1",
    params = List("n: Int")
  )

  @Test def deadUnboxNull = testEquivalence(
    "1",
    "Byte.unbox(null); 1"
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


  @Test def deadCodeFromNullCheckOfThis =
    testEquivalence(
      "1",
      "if (this == null) then return 2; 1"
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


  @Test def regression12343 = // https://github.com/scala/bug/issues/12343
    testEquivalence(
      "()",
      "val limit = 5; var n = 1; var (d, f) = progress match { case 0 => (10000L, 5); case _ => (250L, 4) }; ()",
      params = List("terminated: => Boolean", "progress: Int = 0", "label: => String = \"test\""),
      returnType = "Unit"
    )

  @Test def regression618 =
    val source =
      """trait T {
        |  final def m1 = 1 // trivial
        |  final def m2 = p // forwarder
        |  @noinline def p = 42
        |}
        |
        |object TT extends T // gets mixin forwarders m1 / m2 which call the static T.m1$ / T.m2$
        |
        |class C {
        |  def t1a(t: T) = t.m1 // inlined, so we get 1
        |  def t1b = TT.m1      // mixin forwarder is inlined, static forwarder then as well because the final method is trivial
        |  def t2a(t: T) = t.m2 // inlined, so we get T.p
        |  def t2b = TT.m2      // mixin forwarder is inlined, static forwarder then as well because the final method is forwarder
        |}
      """.stripMargin
    checkBCode(source) { dir =>
      val clsIn = dir.lookupName("C.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val meth1a = getMethod(clsNode, "t1a")
      val meth1b = getMethod(clsNode, "t1b")
      val meth2a = getMethod(clsNode, "t2a")
      val meth2b = getMethod(clsNode, "t2b")
      assert(instructionsFromMethod(meth1a).forall(i => !i.isInstanceOf[ASMConverters.Invoke]), "t1a should not have calls")
      assert(instructionsFromMethod(meth1b).forall(i => !i.isInstanceOf[ASMConverters.Invoke]), "t1b should not have calls")
      assert(instructionsFromMethod(meth2a).forall {
        case ASMConverters.Invoke(_, owner, name, _, _) => owner == "T" && name == "p"
        case _ => true
      }, "t2a should only call T.p")
      assert(instructionsFromMethod(meth2b).forall {
        case ASMConverters.Invoke(_, owner, name, _, _) => owner == "T" && name == "p"
        case _ => true
      }, "t2b should only call T.p")
    }

  @Test def t2171 =
    testEquivalence(
      "while(true) {}",
      "while(true) m(\"...\")",
      returnType = "Unit",
      extraMemberSources = List("final def m(msg: => String) = try 0 catch { case ex: Throwable => println(msg) }")
    )


  private def assertNoCallsExcept(body: String, allowedCalls: (String, String) => Boolean, extraMemberSources: List[String]): Unit =
    val source =
      f"""
         |class Test {
         |  ${extraMemberSources.mkString("\n")}
         |  def test$body
         |}
         """.stripMargin

    checkBCode(source) { dir =>
      val clsIn = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val meth = getMethod(clsNode, "test")
      val instructions = instructionsFromMethod(meth)
      for instr <- instructions do instr match {
        case ASMConverters.Invoke(_, owner, name, _, _) => assert(allowedCalls(owner, name), s"Found call to $owner.$name")
        case ASMConverters.InvokeDynamic(_, name, _, _, _) => assert(false, s"Found unexpected invokedynamic (to $name)")
        case _ => ()
      }
    }

  private def assertNoCallsExcept(body: String, allowedCalls: Set[String], extraMemberSources: List[String] = Nil): Unit =
    assertNoCallsExcept(body, (_, n) => allowedCalls.contains(n), extraMemberSources)

  private def assertNoCallsToClasses(body: String, disallowedClasses: Set[String], extraMemberSources: List[String] = Nil): Unit =
    assertNoCallsExcept(body, (o, _) => disallowedClasses.forall(c => !o.contains(c)), extraMemberSources)

  private def assertNoBoxing(body: String, extraMemberSources: List[String] = Nil): Unit =
    assertNoCallsToClasses(body, Set("BoxesRunTime"), extraMemberSources)

  private def assertNoCalls(body: String, extraMemberSources: List[String] = Nil): Unit =
    assertNoCallsExcept(body, (_, _) => false, extraMemberSources)

  @Test def cleanArrayForeachVal =
    assertNoCallsExcept(
      "(a: Array[Int]) = a.foreach(consume)",
      Set("consume"),
      extraMemberSources = List("def consume(i: Int): Unit = ()")
    )

  @Test def cleanArrayForeachRef =
    assertNoCallsExcept(
      "(a: Array[String]) = a.foreach(_.trim)",
      Set("trim")
    )

  @Test def cleanArrayMapVal =
    assertNoCalls(
      "(a: Array[Int]) = a.map(_ + 1)"
    )

  @Test def cleanArrayMapVal2 =
    assertNoCalls(
      "(a: Array[Byte]): Array[Int] = a.map(_ + 1)"
    )

  @Test def cleanArrayMapVal3 =
    assertNoCalls(
      "(a: Array[Byte]): Array[Byte] = a.map(x => (x + 1).toByte)"
    )

  @Test def cleanArrayMapRef =
    assertNoCallsExcept(
      "(a: Array[String]) = a.map(_.trim)",
      Set("trim")
    )

  @Test def cleanArrayExistsVal = // also covers "forall", "indexWhere", "find" (implemented similarly)
    assertNoBoxing(
      "(a: Array[Int]) = a.exists(_ == 0)"
    )

  @Test def cleanArrayFoldLeftVal = // also covers "fold", "foldRight"
    assertNoCallsExcept(
      "(a: Array[Int]) = a.foldLeft(0)(_ + _)",
      Set("<init>") // exception constructor
    )

  @Test def cleanArrayFoldLeftRef =
    assertNoCallsExcept(
      "(a: Array[String]) = a.foldLeft(0)(_ + _.length)",
      Set("<init>", "length") // exception constructor
    )

  @Test def cleanArrayScanLeftVal = // also covers "scan", "scanRight"
    assertNoCalls(
      "(a: Array[Int]) = a.scanLeft(0)(_ + _)"
    )

  @Test def cleanArrayScanLeftRef =
    assertNoCallsExcept(
      "(a: Array[String]) = a.scanLeft(0)(_ + _.length)",
      Set("length")
    )

  @Test def cleanArrayScanLeftRef2 =
    assertNoCallsExcept(
      "(a: Array[String]) = a.scanLeft(\"\")((_, s) => s.trim)",
      Set("trim")
    )

  @Test def cleanArrayMapInPlaceVal =
    assertNoCalls(
      "(a: Array[Int]) = a.mapInPlace(_ + 1)"
    )

  @Test def cleanArrayMapInPlaceRef =
    assertNoCallsExcept(
      "(a: Array[String]) = a.mapInPlace(_.trim)",
      Set("trim")
    )

  @Test def cleanArrayCountVal =
    assertNoBoxing(
      "(a: Array[Int]) = a.count(_ == 0)"
    )

  @Test def cleanArrayFill =
    assertNoCalls(
      "() = Array.fill[Option[Any]](10)(None)"
    )

  @Test def cleanRangeForeach =
    assertNoCallsToClasses(
      "() = { var x = 0; for i <- 1 to 10 do x += i; x }",
      Set("IntRef")
    )

  // TODO: once we have an inliner and thus CLI args, look into t11255 from scala/scala's test/files/run (https://github.com/scala/scala/pull/7438)
}
