package xsbt

import xsbti.UseScope

import org.junit.{ Test, Ignore }
import org.junit.Assert._

class ExtractUsedNamesSpecification {

  @Test
  def extractImportedName = {
    val src = """package a { class A }
                |package b {
                | import a.{A => A2}
                |}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(src)
    val expectedNames = standardNames ++ Set("a", "A", "A2", "b")
    // names used at top level are attributed to the first class defined in a compilation unit

    assertEquals(usedNames("a.A"), expectedNames)
  }

  // test covers https://github.com/gkossakowski/sbt/issues/6
  @Test
  def extractNameInTypeTree = {
    val srcA = """|package a {
                  |  class A {
                  |    class C { class D }
                  |  }
                  |  class B[T]
                  |}
                  |package c {
                  |  class BB
                  |}
                  |
                  |""".stripMargin
    val srcB = """|package b {
                  | abstract class X {
                  |     def foo: a.A#C#D
                  |     def bar: a.B[c.BB]
                  |   }
                  |}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(srcA, srcB)
    val expectedNames = standardNames ++ Set("a", "c", "A", "B", "C", "D", "b", "X", "BB")
    assertEquals(usedNames("b.X"), expectedNames)
  }

  // test for https://github.com/gkossakowski/sbt/issues/5
  @Test
  def extractSymbolicNames = {
    val srcA = """|class A {
                  |  def `=`: Int = 3
                  |}""".stripMargin
    val srcB = """|class B {
                  |  def foo(a: A) = a.`=`
                  |}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(srcA, srcB)
    val expectedNames = standardNames ++ Set("A", "a", "B", "=", "Int")
    assertEquals(usedNames("B"), expectedNames)
  }

  @Test
  def extractTypeNamesForObjectsDependingOnAbstractTypes = {
    val srcA =
      """abstract class A {
        | type T
        | object X {
        |    def foo(x: T): T = x
        |  }
        |}
      """.stripMargin
    val srcB = "class B extends A { type T = Int }"
    val srcC = "object C extends B"
    val srcD = "object D { C.X.foo(12) }"
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(srcA, srcB, srcC, srcD)
    val scalaVersion = scala.util.Properties.versionNumberString
    // TODO: Find out what's making these types appear in 2.10
    // They don't come from type dependency traverser, but from `addSymbol`
    val versionDependentNames =
      if (scalaVersion.contains("2.10")) Set("Nothing", "Any") else Set()
    val namesA = standardNames ++ Set("A") ++ versionDependentNames
    val namesAX = standardNames ++ Set("X", "x", "T", "A")
    val namesB = Set("B", "A", "Int", "A;init;", "scala")
    val namesC = Set("B;init;", "C", "B")
    val namesD = standardNames ++ Set("D", "C", "X", "foo", "Int", "T")
    assertEquals(usedNames("A"),   namesA)
    assertEquals(usedNames("A.X"), namesAX)
    assertEquals(usedNames("B"),   namesB)
    assertEquals(usedNames("C"),   namesC)
    assertEquals(usedNames("D"),   namesD)
  }

  // See source-dependencies/types-in-used-names-a for an example where
  // this is required.
  @Test
  def extractUsedNamesInTypeOfTree = {
    val src1 = """|class X0
                  |class X1 extends X0
                  |class Y
                  |class A {
                  |  type T >: X1 <: X0
                  |}
                  |class M
                  |class N
                  |class P0
                  |class P1 extends P0
                  |object B {
                  |  type S = Y
                  |  val lista: List[A] = ???
                  |  val at: A#T = ???
                  |  val as: S = ???
                  |  def foo(m: M): N = ???
                  |  def bar[Param >: P1 <: P0](p: Param): Param = ???
                  |}""".stripMargin
    val src2 = """|object Test_lista {
                  |  val x = B.lista
                  |}
                  |object Test_at {
                  |  val x = B.at
                  |}
                  |object Test_as {
                  |  val x = B.as
                  |}
                  |object Test_foo {
                  |  val x = B.foo(???)
                  |}
                  |object Test_bar {
                  |  val x = B.bar(???)
                  |}
                  |""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(src1, src2)
    val expectedNames_lista = standardNames ++ Set("Test_lista", "x", "B", "lista", "List", "A")
    val expectedNames_at = standardNames ++ Set("Test_at", "x", "B", "at", "A", "T", "X0", "X1")
    val expectedNames_as = standardNames ++ Set("Test_as", "x", "B", "as", "S", "Y")
    val expectedNames_foo = standardNames ++ Set("Test_foo",
                                                 "x",
                                                 "B",
                                                 "foo",
                                                 "M",
                                                 "N",
                                                 "Predef",
                                                 "???",
                                                 "Nothing")
    val expectedNames_bar = standardNames ++ Set("Test_bar",
                                                 "x",
                                                 "B",
                                                 "bar",
                                                 "Param",
                                                 "P1",
                                                 "P0",
                                                 "Predef",
                                                 "???",
                                                 "Nothing")
    assertEquals(usedNames("Test_lista"), expectedNames_lista)
    assertEquals(usedNames("Test_at"),    expectedNames_at)
    assertEquals(usedNames("Test_as"),    expectedNames_as)
    assertEquals(usedNames("Test_foo"),   expectedNames_foo)
    assertEquals(usedNames("Test_bar"),   expectedNames_bar)
  }

  @Test
  def extractUsedNamesFromExistential = {
    val srcFoo =
      """import scala.language.existentials
      |class Foo {
      |  val foo: T forSome { type T <: Double } = ???
      |}
      """.stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(srcFoo)
    val expectedNames = standardNames ++ Seq("Double",
                                             "Foo",
                                             "T",
                                             "foo",
                                             "scala",
                                             "language",
                                             "existentials",
                                             "Nothing",
                                             "???",
                                             "Predef")
    assertEquals(usedNames("Foo"), expectedNames)
  }

  @Test
  def extractUsedNamesFromRefinement = {
    val srcFoo =
      "object Outer {\n  class Inner { type Xyz }\n\n  type TypeInner = Inner { type Xyz = Int }\n}"
    val srcBar = "object Bar {\n  def bar: Outer.TypeInner = null\n}"
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(srcFoo, srcBar)
    val expectedNames = standardNames ++ Set("Bar", "Outer", "TypeInner", "Inner", "Xyz", "Int")
    assertEquals(usedNames("Bar"), expectedNames)
  }

  // test for https://github.com/gkossakowski/sbt/issues/3
  @Test
  def extractUsedNamesFromSameCompilationUnit = {
    val src = "class A { def foo: Int = 0; def bar: Int = foo }"
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(src)
    val expectedNames = standardNames ++ Set("A", "foo", "Int")
    assertEquals(usedNames("A"), expectedNames)
  }

  // pending test for https://issues.scala-lang.org/browse/SI-7173
  @Test
  def extractUsedNamesOfConstants = {
    val src = "class A { final val foo = 12; def bar: Int = foo }"
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(src)
    val expectedNames = standardNames ++ Set("A", "foo", "Int")
    assertEquals(usedNames("A"), expectedNames)
  }

  // test for https://github.com/gkossakowski/sbt/issues/4
  // TODO: we should fix it by having special treatment of `selectDynamic` and `applyDynamic` calls
  @Test
  def extractNamesFromMethodCallOnDynamic = {
    val srcA = """|import scala.language.dynamics
                  |class A extends Dynamic {
                  | def selectDynamic(name: String): Int = name.length
                  |}""".stripMargin
    val srcB = "class B { def foo(a: A): Int = a.bla }"
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(srcA, srcB)
    val expectedNames = standardNames ++ Set("B", "A", "a", "Int", "selectDynamic", "bla")
    assertEquals(usedNames("B"), expectedNames)
  }

  @Test
  def extractSealedClassScope = {
    val sealedClassName = "Sealed"
    val sealedClass =
      s"""package base
        |
        |sealed class $sealedClassName
        |object Usage extends $sealedClassName
        |object Usage2 extends $sealedClassName
      """.stripMargin

    def findPatMatUsages(in: String): Set[String] = {
      val compilerForTesting = new ScalaCompilerForUnitTesting
      val (_, callback) =
        compilerForTesting.compileSrcs(List(List(sealedClass, in)), reuseCompilerInstance = false)
      val clientNames = callback.usedNamesAndScopes.filterKeys(!_.startsWith("base."))

      val names: Set[String] = clientNames.flatMap {
        case (_, usages) =>
          usages.filter(_.scopes.contains(UseScope.PatMatTarget)).map(_.name)
      }(collection.breakOut)

      names
    }

    def classWithPatMatOfType(tpe: String = sealedClassName) =
      s"""package client
        |import base._
        |
        |class test(a: $tpe) {
        |  a match {
        |   case _ => 1
        |  }
        |}
      """.stripMargin

    assertEquals(findPatMatUsages(classWithPatMatOfType()), Set(sealedClassName))
    // Option is sealed
    assertEquals(findPatMatUsages(classWithPatMatOfType(s"Option[$sealedClassName]")),
      Set(sealedClassName, "Option"))
    // Seq and Set is not
    assertEquals(findPatMatUsages(classWithPatMatOfType(s"Seq[Set[$sealedClassName]]")), Set(sealedClassName))

    def inNestedCase(tpe: String) =
      s"""package client
          |import base._
          |
          |class test(a: Any) {
          |  a match {
          |   case _: $tpe => 1
          |  }
          |}""".stripMargin

    assertEquals(findPatMatUsages(inNestedCase(sealedClassName)), Set())

    val notUsedInPatternMatch =
      s"""package client
          |import base._
          |
          |class test(a: Any) {
          |  a match {
          |   case _ => 1
          |  }
          |  val aa: $sealedClassName = ???
          |}""".stripMargin

    assertEquals(findPatMatUsages(notUsedInPatternMatch), Set())
  }

  /**
   * Standard names that appear in every compilation unit that has any class
   * definition.
   */
  private val standardNames = Set(
    "scala",
    // The default parent of a class is "AnyRef" which is an alias for "Object"
    "AnyRef",
    "Object",
    "java.lang.Object;init;"
  )

}