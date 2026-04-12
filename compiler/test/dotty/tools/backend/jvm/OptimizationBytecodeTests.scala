package dotty.tools.backend.jvm

import dotty.{AsmConverters, DottyBytecodeTest}
import dotty.AsmConverters.instructionsFromMethod
import dotty.tools.dotc.config.Settings.Setting.ChoiceWithHelp
import org.junit.Test

class OptimizationBytecodeTests extends DottyBytecodeTest {
  override def initCtx = {
    val ctx = super.initCtx
    ctx.setSetting(ctx.settings.opt, true)
    ctx.setSetting(ctx.settings.optInline, List("**", "!java.**"))
    // Probably don't:
    //ctx.setSetting(ctx.settings.YoptInlineHeuristics, "everything")
    // For debugging purposes:
    //ctx.setSetting(ctx.settings.Ydebug, true)
    //ctx.setSetting(ctx.settings.silentWarnings, false)
    //ctx.setSetting(ctx.settings.Wopt, List(ChoiceWithHelp("all", "")))
    //ctx.setSetting(ctx.settings.YoptLogInline, "_")
    //ctx.setSetting(ctx.settings.YoptTrace, "_")
  }


  private def escapeSource(fullSource: String): String = {
    // for easier debugging via cleaner logs, only generate the escape method if it's actually used
    if fullSource.contains("escape")
    then
      """
        |// Because our elimination of load-store pairs isn't amazing, we define escaping as a purely static function so there's no module/this load
        |// The point of the tests with escaping is not to test exactly how escaping itself is compiled
        |object escape { @noinline @annotation.static def apply(a: Any): Unit = ??? }
        |class escape { }
        |""".stripMargin
    else
      ""
  }

  def assertEquivalence(expectedSource: String, actualSource: String, params: List[String] = Nil, extraMemberSources: List[String] = Nil, returnType: String = "Int"): Unit = {
    val source =
      f"""
        |${escapeSource(expectedSource + actualSource + extraMemberSources.mkString("\n"))}
        |final class Test {
        |  ${extraMemberSources.mkString("\n  ")}
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
  
  private def assertCalls(allowedCalls: (String, String) => Boolean, body: String, params: List[String] = Nil, extraMemberSources: List[String] = Nil, returnType: String = "Int"): Unit =
    val source =
      f"""
         |${escapeSource(body + extraMemberSources.mkString("\n"))}
         |final class Test {
         |  ${extraMemberSources.mkString("\n  ")}
         |  def test(${params.mkString(", ")}): $returnType = { $body }
         |}
         """.stripMargin

    checkBCode(source) { dir =>
      val clsIn = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val meth = getMethod(clsNode, "test")
      val instructions = instructionsFromMethod(meth)
      for instr <- instructions do instr match {
        case AsmConverters.Invoke(_, owner, name, _, _) => assert(allowedCalls(owner, name), s"Found invoke to $owner.$name in:\n${instructions.mkString("\n")}")
        case AsmConverters.InvokeDynamic(_, name, _, _, _) => assert(false, s"Found dynamic invoke to $name in:\n${instructions.mkString("\n")}")
        case _ => ()
      }
    }

  object Calls {
    def none(clazz: String, meth: String): Boolean = false
    def noneToClasses(disallowedClasses: String*)(clazz: String, meth: String): Boolean = disallowedClasses.forall(c => !clazz.contains(c))
    def noBoxing: (String, String) => Boolean = noneToClasses("BoxesRunTime")
    def noneExcept(allowedCalls: String*)(clazz: String, meth: String): Boolean = allowedCalls.contains(meth)
  }


  @Test def inlineTuple2Specialized =
    assertEquivalence(
      "1",
      "val t = (1, 2); t._1"
    )

  @Test def inlineTuple2SpecializedTwice =
    assertEquivalence(
      "1",
      "val t = (1, 2); val t2 = (3, t._1); t2._2"
    )

  @Test def inlineTuple2OfTuple2Specialized =
    // we'd like `assertEquivalence("1 + 2 + 3", ` but our simple store-load elimination can't do it
    assertCalls(Calls.none,
      "val ((a, b), c) = ((1, 2), 3); a + b + c"
    )

  @Test def inlineTuple2SpecializedCastToGeneral =
    assertEquivalence(
      "3",
      "val t = (3, 4); (t: Tuple2[Any, Any])._1.asInstanceOf[Int]"
    )

  @Test def inlineTuple3Specialized =
    assertEquivalence(
      "1",
      "val t = (3, 1, 2); t._2"
    )

  @Test def inlineTuple2General =
    assertEquivalence(
      "x",
      "val a: Any = x; val t = (a, 42); t._1.asInstanceOf[Int]",
      params = List("x: Int"),
    )

  @Test def inlineTuple2OfTuple2General =
    // we'd like `assertEquivalence("1 + 2 + 3", ` but our simple store-load elimination can't do it
    assertCalls(Calls.none,
      "val ((a, b), c) = ((1, 2), Integer.valueOf(3)); a + b + c"
    )

  @Test def inlineTuple2GeneralAndSpecialized =
    assertEquivalence(
      "x",
      "val t1 = (x, 0); val a: Any = t1._1; val t2 = (a, 42); t2._1.asInstanceOf[Int]",
      params = List("x: Int")
    )


  @Test def inlineNonGenericLocallyFunction =
    assertEquivalence(
      "1",
      "locally { 1 }",
      extraMemberSources = List("@inline def locally(x: Int): Int = x") // simplified from the form below
    )

  @Test def inlineGenericLocallyFunction =
    assertEquivalence(
      "1",
      "locally { 1 }",
      extraMemberSources = List("@inline def locally[T](@deprecatedName(\"x\") x: T): T = x") // copied from Predef.scala
    )


  @Test def inlineNn =
    assertEquivalence(
      "if x == null then throw new NullPointerException(\"tried to cast away nullability, but value is null\"); x.asInstanceOf[Int]",
      "x.nn",
      params = List("x: Int | Null")
    )


  @Test def inlineUnusedAdditionButKeepNullCheck =
    assertEquivalence(
      "x.nn; 0",
      "add(x.nn, 0); 0",
      extraMemberSources = List("def add(x: Int, y: Int): Int = x + y"),
      params = List("x: Int | Null")
    )


  @Test def inlineBoxUnbox =
    assertEquivalence(
      "x",
      "val a: Any = x; a.asInstanceOf[Int]",
      params = List("x: Int")
    )

  @Test def inlineBoxUnboxMixedArithmetic =
    assertEquivalence(
      "x + y",
      "val a: java.lang.Integer = x; val b: java.lang.Long = y; a + b",
      params = List("x: Int", "y: Long"),
      returnType = "Long"
    )

  @Test def inlineBoxUnboxValEscaped =
    assertEquivalence(
      "escape(x); x",
      "val a: Any = x; escape(a); a.asInstanceOf[Int]",
      params = List("x: Int")
    )

  @Test def inlineBoxUnboxValEscapedBranch =
    assertCalls(Calls.noBoxing,
      "val i = Integer.valueOf(10); val j = Integer.valueOf(32); escape(if b then i else j); if b then i.toInt else j.toInt",
      params = List("b: Boolean")
    )

  @Test def inlineBoxUnboxVarClosure =
    assertEquivalence(
      "x",
      "var a: Any = x; val c: () => String = () => a.asInstanceOf[String]; c()",
      params = List("x: String"),
      returnType = "String"
    )

  @Test def inlineBoxUnboxBranch =
    assertEquivalence(
      "if x >= 0 then 1 else 2",
      "var a = if x >= 0 then (1: Any) else 2; a.asInstanceOf[Int]",
      params = List("x: Int")
    )

  @Test def inlineBoxUnboxBranchContrived =
    assertEquivalence(
      "String.valueOf(if x > 0 then x else -x)",
      "val wat: Any = if x > 0 then x else -x; wat match { case x: Int => String.valueOf(x); case _ => \"?\" }",
      params = List("x: Int"),
      returnType = "String"
    )

  @Test def inlineBoxUnboxJava =
    assertEquivalence(
      "x",
      "val a = java.lang.Integer.valueOf(x); a.asInstanceOf[Int]",
      params = List("x: Int")
    )

  @Test def inlineBoxUnboxJavaBranch =
    assertEquivalence(
      "10",
      "val b = java.lang.Boolean.valueOf(true); val a = if b then java.lang.Integer.valueOf(10) else 0; a.asInstanceOf[Int]"
    )

  @Test def inlineUnboxNull =
    assertEquivalence(
      "0",
      "scala.runtime.BoxesRunTime.unboxToInt(null)"
    )

  @Test def inlineUnboxNull2 =
    assertEquivalence(
      "0",
      "null.asInstanceOf[Int]"
    )


  @Test def inlineRef =
    assertEquivalence(
      "0",
      "val r = scala.runtime.IntRef(0); r.elem"
    )

  @Test def inlineRefBranch =
    // we'd like assertEquivalence("if b then 0 else 1", ` but our simple store-load elimination can't do it
    assertCalls(Calls.none,
      "val r1 = scala.runtime.IntRef.zero(); val r2 = scala.runtime.IntRef.create(1); val r = if b then r1 else r2; r.elem",
      params = List("b: Boolean")
    )

  @Test def inlineRefWrite =
    assertEquivalence(
      "var l = 10L; l += 3; l",
      "val r = scala.runtime.LongRef.create(10); r.elem += 3; r.elem",
      returnType = "Long"
    )

  @Test def inlineRefUsedAsBranch =
    // we'd like `assertEquivalence("if b then 1 else 2", ` but our simple store-load elimination can't do it
    assertCalls(Calls.none,
      "val x = scala.runtime.BooleanRef.create(false); if b then x.elem = true; if x.elem then 1 else 2",
      params = List("b: Boolean")
    )


  @Test def inlineMatch =
    assertEquivalence(
      "a + b",
      "(a, b) match { case (a: Int, b: Int) => a + b; case _ => -1 }",
      params = List("a: Int", "b: Int")
    )

  @Test def inlineMatchBoxed =
    assertEquivalence(
      "a + b",
      "(a: Any, b: Any) match { case (a: Int, b: Int) => a + b; case _ => -1 }",
      params = List("a: Int", "b: Int")
    )

  @Test def inlineMatchResult =
    // we'd like assertEquivalence("if n == 0 then 1 + 2 else 3 + 4", ` but our simple store-load elimination can't do it
    assertCalls(Calls.none,
      "val (a, b) = n match { case 0 => (1, 2); case _ => (3, 4) }; a + b",
      params = List("n: Int")
    )


  @Test def inlineClosure =
    // we'd like assertEquivalence("42", ` but our simple store-load elimination can't do it
    assertCalls(Calls.none,
      "val x = 10; val c: Int => Int = y => y + x; c(32)"
    )


  @Test def aliasResolution =
    assertEquivalence(
      "x.length",
      "val y = x; y.length",
      params = List("x: Array[Int]")
    )


  @Test def loadAfterStore =
    assertEquivalence(
      "1",
      "val x = 1; x"
    )

  @Test def loadAfterStoreNull =
    assertEquivalence(
      "null",
      "val x: Int | Null = null; x",
      returnType = "Int | Null"
    )

  @Test def loadAfterStoreTwice =
    assertEquivalence(
      "1",
      "val x = 1; val y = x; y"
    )


  @Test def deadStoreVal =
    assertEquivalence(
      "1",
      "val x = 42; 1"
    )

  @Test def deadStoreVar =
    assertEquivalence(
      "1",
      "var x = 42; 1"
    )

  @Test def deadStoreDueToSubsequentWriteLocal =
    assertEquivalence(
      "1",
      "var x = 42; x = 1; x"
    )

  @Test def deadStoreDespiteRead =
    assertEquivalence(
      "1",
      "var x = 42; val y = x; 1"
    )

  @Test def deadStoreDespiteCall =
    assertEquivalence(
      "1",
      "def eat(x: Int): Unit = (); var x = 42; eat(x); 1"
    )

  @Test def deadTryCatch =
    assertEquivalence(
      "1",
      "var x = 1; try () catch case _ => (); x"
    )


  @Test def elidedCastPrimitive =
    assertEquivalence(
      "x",
      "x.asInstanceOf[Int]",
      params = List("x: Int")
    )

  @Test def elidedCastClass =
    assertEquivalence(
      "x",
      "x.asInstanceOf[String]",
      params = List("x: String"),
      returnType = "String"
    )

  @Test def elidedIsInstanceTop =
    assertEquivalence(
      "true",
      "val b = (x: Integer); b.isInstanceOf[Object]",
      params = List("x: Int"),
      returnType = "Boolean"
    )

  @Test def elidedIsInstanceInterface =
    assertEquivalence(
      "true",
      "val b = (x: Integer); b.isInstanceOf[Number]",
      params = List("x: Int"),
      returnType = "Boolean"
    )


  @Test def deadBoxJava =
    assertEquivalence(
      "x",
      "java.lang.Integer.valueOf(x); x",
      params = List("x: Int")
    )

  @Test def deadBoxScala =
    assertEquivalence(
      "x",
      "scala.runtime.BoxesRunTime.boxToInteger(x); x",
      params = List("x: Int")
    )


  @Test def deadUnbox =
    assertEquivalence(
      "1",
      "Byte.unbox(b); 1",
      params = List("b: java.lang.Byte")
    )

  @Test def deadUnboxBox =
    assertEquivalence(
      "1",
      "Int.unbox(Integer.valueOf(n)); 1",
      params = List("n: Int")
    )

  @Test def deadUnboxNull =
    assertEquivalence(
      "1",
      "Byte.unbox(null); 1"
    )


  @Test def deadCodeAfterReturn =
    assertEquivalence(
      "1",
      "return 1; 2"
    )

  @Test def deadCodeAfterThrow =
    assertEquivalence(
      "throw new Error()",
      "throw new Error(); 2"
    )


  @Test def deadCodeFromNullCheckOfThis =
    assertEquivalence(
      "1",
      "if (this == null) then return 2; 1"
    )

  @Test def deadCodeFromNullCheckOfConstantNull =
    assertEquivalence(
      "2",
      "val x: String | Null = null; if (x == null) then return 2; 1"
    )

  @Test def deadCodeFromNonNullCheckOfConstantNull =
    assertEquivalence(
      "1",
      "val x: String | Null = null; if (x != null) then return 2; 1"
    )

  @Test def deadCodeFromEqNullCheckOfConstantNull =
    assertEquivalence(
      "2",
      "val x: String | Null = null; val y: String | Null = null; if (x == y) then return 2; 1"
    )

  @Test def deadCodeFromNeqNullCheckOfConstantNull =
    assertEquivalence(
      "1",
      "val x: String | Null = null; val y: String | Null = null; if (x != y) then return 2; 1"
    )


  @Test def deadCodeFromNullCheckOfConstantString =
    assertEquivalence(
      "1",
      "val x: String | Null = \"\"; if (x == null) then return 2; 1"
    )

  @Test def deadCodeFromNonNullCheckOfConstantString =
    assertEquivalence(
      "2",
      "val x: String | Null = \"\"; if (x != null) then return 2; 1"
    )

  @Test def deadCodeFromEqNullCheckOfConstantString =
    assertEquivalence(
      "1",
      "val x: String | Null = \"\"; val y: String | Null = null; if (x == y) then return 2; 1"
    )

  @Test def deadCodeFromNeqNullCheckOfConstantString =
    assertEquivalence(
      "2",
      "val x: String | Null = \"\"; val y: String | Null = null; if (x != y) then return 2; 1"
    )


  @Test def deadCodeFromIsInstanceExact =
    assertEquivalence(
      "if x == null then 1 else 2",
      "if x == null then 1 else if x.isInstanceOf[String] then 2 else 3",
      params = List("x: String | Null")
    )

  @Test def deadCodeFromIsInstanceAny =
    assertEquivalence(
      "if x == null then 1 else 2",
      "if x == null then 1 else if x.isInstanceOf[Any] then 2 else 3",
      params = List("x: String | Null")
    )

  @Test def deadCodeFromIsInstanceNull =
    assertEquivalence(
      "2",
      "var y = x; y = null; if y.isInstanceOf[String] then 1 else 2",
      params = List("x: String | Null")
    )

  @Test def deadCodeFromIsInstanceUnrelated =
    assertEquivalence(
      "2",
      "if x.isInstanceOf[List[Int]] then 1 else 2",
      params = List("x: String")
    )


  @Test def regression12343 = // https://github.com/scala/bug/issues/12343
    assertEquivalence(
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
      val instrs1a = instructionsFromMethod(meth1a)
      val instrs1b = instructionsFromMethod(meth1b)
      val instrs2a = instructionsFromMethod(meth2a)
      val instrs2b = instructionsFromMethod(meth2b)
      assert(instrs1a.forall(i => !i.isInstanceOf[AsmConverters.Invoke]), "t1a should not have calls, it has:\n" + instrs1a.mkString("\n"))
      assert(instrs1b.forall(i => !i.isInstanceOf[AsmConverters.Invoke]), "t1b should not have calls, it has:\n" + instrs1b.mkString("\n"))
      assert(instrs2a.forall {
        case AsmConverters.Invoke(_, owner, name, _, _) => owner == "T" && name == "p"
        case _ => true
      }, "t2a should only call T.p, but instead:\n" + instrs2a.mkString("\n"))
      assert(instrs2b.forall {
        case AsmConverters.Invoke(_, owner, name, _, _) => owner == "T" && name == "p"
        case _ => true
      }, "t2b should only call T.p, but instead:\n" + instrs2b.mkString("\n"))
    }

  @Test def t2171 =
    assertEquivalence(
      "while(true) {}",
      "while(true) m(\"...\")",
      returnType = "Unit",
      extraMemberSources = List("final def m(msg: => String) = try () catch { case ex: Throwable => println(msg) }")
    )


  @Test def cleanArrayForeachVal =
    assertEquivalence(
      "var l = a.length; var i = 0; while i < l do { escape(a(i)); i += 1 }",
      "a.foreach(i => escape(i))",
      params = List("a: Array[Int]"),
      returnType = "Unit"
    )

  @Test def cleanArrayForeachRef =
    assertEquivalence(
      "var l = a.length; var i = 0; while i < l do { a(i).trim(); i += 1 }",
      "a.foreach(_.trim)",
      params = List("a: Array[String]"),
      returnType = "Unit"
    )

  @Test def cleanArrayMapVal =
    assertEquivalence(
      "var l = a.length; var r = new Array[Int](l); if l > 0 then { var i = 0; while i < l do { val temp = a(i) + 1; r(i) = temp; i += 1 } }; r",
      "a.map(_ + 1)",
      params = List("a: Array[Int]"),
      returnType = "Array[Int]"
    )

  @Test def cleanArrayMapVal2 =
    assertEquivalence(
      "var l = a.length; var r = new Array[Int](l); if l > 0 then { var i = 0; while i < l do { val temp = a(i) + 1; r(i) = temp; i += 1 } }; r",
      "a.map(_ + 1)",
      params = List("a: Array[Byte]"),
      returnType = "Array[Int]"
    )

  @Test def cleanArrayMapVal3 =
    assertEquivalence(
      "var l = a.length; var r = new Array[Byte](l); if l > 0 then { var i = 0; while i < l do { val temp = (a(i) + 1).toByte; r(i) = temp; i += 1 } }; r",
      "a.map(x => (x + 1).toByte)",
      params = List("a: Array[Byte]"),
      returnType = "Array[Byte]"
    )

  @Test def cleanArrayMapRef =
    assertEquivalence(
      "var l = a.length; var r = new Array[String](l); if l > 0 then { var i = 0; while i < l do { var temp = a(i).trim; r(i) = temp; temp = null; i += 1 } }; r",
      "a.map(_.trim)",
      params = List("a: Array[String]"),
      returnType = "Array[String]"
    )

  @Test def cleanArrayExistsVal = // also covers "forall", "indexWhere", "find" (implemented similarly)
    assertCalls(Calls.noBoxing,
      "a.exists(_ == 0)",
      params = List("a: Array[Int]"),
      returnType = "Boolean"
    )

  @Test def cleanArrayFoldLeftVal = // also covers "fold", "foldRight"
    assertCalls(Calls.noneExcept("<init>"),  // constructor for the exception
      "a.foldLeft(0)(_ + _)",
      params = List("a: Array[Int]")
    )

  @Test def cleanArrayFoldLeftRef =
    assertCalls(Calls.noneExcept("<init>", "length"), // constructor for the exception
      "a.foldLeft(0)(_ + _.length)",
      params = List("a: Array[String]")
    )

  @Test def cleanArrayScanLeftVal = // also covers "scan", "scanRight"
    assertCalls(Calls.none,
      "a.scanLeft(0)(_ + _)",
      params = List("a: Array[Int]"),
      returnType = "Array[Int]"
    )

  @Test def cleanArrayScanLeftRef =
    assertCalls(Calls.noneExcept("length"),
      "a.scanLeft(0)(_ + _.length)",
      params = List("a: Array[String]"),
      returnType = "Array[Int]"
    )

  @Test def cleanArrayScanLeftRef2 =
    assertCalls(Calls.noneExcept("trim"),
      "a.scanLeft(\"\")((_, s) => s.trim)",
      params = List("a: Array[String]"),
      returnType = "Array[String]"
    )

  @Test def cleanArrayMapInPlaceVal =
    assertEquivalence(
      "var i = 0; while i < a.length do { val temp = a(i) + 1; a(i) = temp; i += 1 }; a",
      "a.mapInPlace(_ + 1)",
      params = List("a: Array[Int]"),
      returnType = "Array[Int]"
    )

  @Test def cleanArrayMapInPlaceRef =
    assertEquivalence(
      "var i = 0; while i < a.length do { var temp = a(i).trim; a(i) = temp; temp = null; i += 1 }; a",
      "a.mapInPlace(_.trim)",
      params = List("a: Array[String]"),
      returnType = "Array[String]"
    )

  @Test def cleanArrayCountVal =
    assertCalls(Calls.noBoxing,
      "a.count(_ == 0)",
      params = List("a: Array[Int]")
    )

  @Test def cleanArrayFill =
    assertCalls(Calls.none,
      "Array.fill[Option[Any]](10)(None)",
      returnType = "Array[Option[Any]]"
    )

  @Test def cleanRangeForeach =
    // Ideally we'd like to assert equivalence to "var x = 0; var i = 1; while i <= 10 do { x += i; i += 1 }; x",
    // but we cannot inline 'start', 'isEmpty', etc. in `Range` because the fields are private...
    assertCalls(Calls.noneToClasses("IntRef"),
      "var x = 0; for i <- 1 to 10 do x += i; x"
    )


  @Test def inlineSingleAbstractMethod =
    assertEquivalence(
      "if i eq null then throw null; 42",
      "i.foo()",
      params = List("i: Impl"),
      extraMemberSources = List(
        "abstract class SAM { def foo(): Int }",
        "final class Impl extends SAM { def foo() = 42 }"
      )
    )

  @Test def inlineSingleAbstractMethodLambda =
    assertEquivalence(
      "{ val a = 9; a + 1 } + { val b = 31; b + 1 }",
      "call(n => n + 1)",
      extraMemberSources = List(
        "trait MyIntFunction { def foo(n: Int): Int }",
        "def call(f: MyIntFunction): Int = f.foo(9) + f.foo(31)"
      )
    )


  // Regression tests for bugs found during optimizer development:

  @Test def inlinedForeachBodyNotLost = {
    val source =
      f"""
         |class Foo:
         |    def foo(arr: Array[Int]) = arr.foreach{ n => println(n) }
         """.stripMargin

    checkBCode(source) { dir =>
      val clsIn = dir.lookupName("Foo.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val meth1 = getMethod(clsNode, "foo")

      val instructions1 = instructionsFromMethod(meth1)
      assert(instructions1.exists {
        case inv: AsmConverters.Invoke => inv.name == "println"
        case _ => false
      })
    }
  }

  @Test def doNotInlineJdkInternals = {
    val source =
      f"""
         |class Test {
         |  def bad(): Double =
         |    "Hi".toDouble
         |}
         """.stripMargin

    checkBCode(source) { dir =>
      val clsIn = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)

      for m <- List(getMethod(clsNode, "bad")) do
        val instrs = instructionsFromMethod(m)
        assert(!instrs.exists {
          case inv: AsmConverters.Invoke => inv.owner.startsWith("jdk/internal")
          case _ => false
        })
    }
  }
}
