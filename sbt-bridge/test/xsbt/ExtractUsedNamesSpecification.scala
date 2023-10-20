package xsbt

import xsbti.UseScope
import ScalaCompilerForUnitTesting.Callbacks

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

    assertEquals(expectedNames, usedNames("a.A"))
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
    val expectedNames = standardNames ++ Set("a", "c", "A", "B", "C", "D", "b", "BB")
    assertEquals(expectedNames, usedNames("b.X"))
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
    val expectedNames = standardNames ++ Set("A", "a", "=", "Int")
    assertEquals(expectedNames, usedNames("B"))
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
    val namesA = standardNames ++ Set("Nothing", "Any")
    val namesAX = standardNames ++ Set("x", "T", "A", "Nothing", "Any")
    val namesB = Set("A", "Int", "A;init;", "Unit")
    val namesC = Set("B;init;", "B", "Unit")
    val namesD = standardNames ++ Set("C", "X", "foo", "Int", "T")
    assertEquals(namesA, usedNames("A"))
    assertEquals(namesAX, usedNames("A.X"))
    assertEquals(namesB, usedNames("B"))
    assertEquals(namesC, usedNames("C"))
    assertEquals(namesD, usedNames("D"))
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
    val expectedNames_lista =
      standardNames ++ Set("B", "lista", "List", "A")
    val expectedNames_at =
      standardNames ++ Set("B", "at", "A", "T", "X0", "X1")
    val expectedNames_as =
      standardNames ++ Set("B", "as", "S", "Y")
    val expectedNames_foo =
      standardNames ++
       Set("B",
           "foo",
           "M",
           "N",
           "Predef",
           "???",
           "Nothing")
    val expectedNames_bar =
      standardNames ++
       Set("B",
           "bar",
           "P1",
           "P0",
           "Predef",
           "???",
           "Nothing")
    assertEquals(expectedNames_lista, usedNames("Test_lista"))
    assertEquals(expectedNames_at, usedNames("Test_at"))
    assertEquals(expectedNames_as, usedNames("Test_as"))
    assertEquals(expectedNames_foo, usedNames("Test_foo"))
    assertEquals(expectedNames_bar, usedNames("Test_bar"))
  }

  @Test
  def extractUsedNamesFromRefinement = {
    val srcFoo = """|object Outer {
                    |  class Inner { type Xyz }
                    |  type TypeInner = Inner { type Xyz = Int }
                    |}
                    |""".stripMargin
    val srcBar = """|object Bar {
                    |  def bar: Outer.TypeInner = null
                    |}
                    |""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(srcFoo, srcBar)
    val expectedNames = standardNames ++ Set("Outer", "TypeInner", "Inner", "Int")
    assertEquals(expectedNames, usedNames("Bar"))
  }

  // test for https://github.com/gkossakowski/sbt/issues/3
  @Test
  def extractUsedNamesFromSameCompilationUnit = {
    val src = "class A { def foo: Int = 0; def bar: Int = foo }"
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(src)
    val expectedNames = standardNames ++ Set("A", "foo", "Int")
    assertEquals(expectedNames, usedNames("A"))
  }

  // pending test for https://issues.scala-lang.org/browse/SI-7173
  @Test
  def extractUsedNamesOfConstants = {
    val src = "class A { final val foo = 12; def bar: Int = foo }"
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(src)
    val expectedNames = standardNames ++ Set("A", "foo", "Int")
    assertEquals(expectedNames, usedNames("A"))
  }

  // test for https://github.com/gkossakowski/sbt/issues/4
  // TODO: we should fix it by having special treatment of `selectDynamic` and `applyDynamic` calls
  @Ignore("Call to Dynamic is desugared in type checker so Select nodes is turned into string literal.")
  def extractNamesFromMethodCallOnDynamic = {
    val srcA = """|import scala.language.dynamics
                  |class A extends Dynamic {
                  | def selectDynamic(name: String): Int = name.length
                  |}""".stripMargin
    val srcB = "class B { def foo(a: A): Int = a.bla }"
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(srcA, srcB)
    val expectedNames = standardNames ++ Set("A", "a", "Int", "selectDynamic", "bla")
    assertEquals(expectedNames, usedNames("B"))
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
      val (_, Callbacks(callback, _)) =
        compilerForTesting.compileSrcs(List(List(sealedClass, in)))
      val clientNames = callback.usedNamesAndScopes.view.filterKeys(!_.startsWith("base."))

      val names: Set[String] = clientNames.flatMap {
        case (_, usages) =>
          usages.filter(_.scopes.contains(UseScope.PatMatTarget)).map(_.name)
      }.toSet

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

    assertEquals(Set(sealedClassName), findPatMatUsages(classWithPatMatOfType()))
    // Option is sealed
    assertEquals(Set(sealedClassName, "Option"),
      findPatMatUsages(classWithPatMatOfType(s"Option[$sealedClassName]")))
    // Seq and Set is not
    assertEquals(Set(sealedClassName), findPatMatUsages(classWithPatMatOfType(s"Seq[Set[$sealedClassName]]")))

    def inNestedCase(tpe: String) =
      s"""package client
          |import base._
          |
          |class test(a: Any) {
          |  a match {
          |   case _: $tpe => 1
          |  }
          |}""".stripMargin

    assertEquals(Set(), findPatMatUsages(inNestedCase(sealedClassName)))

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

    assertEquals(Set(), findPatMatUsages(notUsedInPatternMatch))
  }

  @Test
  def extractedNamesInImport = {
    val src =
      """|import java.util.List
         |
         |class Test
      """.stripMargin

    val compilerForTesting = new ScalaCompilerForUnitTesting
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(src)

    val expectedNames = standardNames ++ Set("java", "util", "List")
    assertEquals(expectedNames, usedNames("Test"))
  }

  /**
   * Standard names that appear in every compilation unit that has any class
   * definition.
   */
  private val standardNames = Set(
    // All classes extend Object
    "Object",
    // All classes have a default constructor called <init>
    "java.lang.Object;init;",
    // the return type of the default constructor is Unit
    "Unit"
  )
}
