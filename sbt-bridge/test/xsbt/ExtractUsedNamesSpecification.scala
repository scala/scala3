/** Adapted from https://github.com/sbt/sbt/blob/0.13/compile/interface/src/test/scala/xsbt/ExtractUsedNamesSpecification.scala */
package xsbt

import org.junit.runner.RunWith
import xsbti.api.ClassLike
import xsbti.api.Def
import xsbti.api.Package
import xsbt.api.SameAPI
import org.junit.runners.JUnit4

import org.specs2.mutable.Specification

@RunWith(classOf[JUnit4])
class ExtractUsedNamesSpecification extends Specification {

  /**
   * Standard names that appear in every compilation unit that has any class
   * definition.
   */
  private val standardNames = Set(
    // All class extend Object
    "Object",
    // All class have a default constructor called <init>
    "<init>",
    // the return type of the default constructor is Unit
    "Unit"
  )

  "imported name" in {
    val src = """
			|package a { class A }
			|package b {
			|	import a.{A => A2}
			|}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(src)
    val expectedNames = standardNames ++ Set("a", "A", "A2", "b")
    usedNames === expectedNames
  }

  // test covers https://github.com/gkossakowski/sbt/issues/6
  "names in type tree" in {
    val srcA = """|
			|package a {
			|  class A {
			|    class C { class D }
			|  }
			|  class B[T]
			|  class BB
			|}""".stripMargin
    val srcB = """|
			|package b {
			|	abstract class X {
			|     def foo: a.A#C#D
			|     def bar: a.B[a.BB]
			|   }
			|}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(srcA, srcB)
    // DOTTY: unlike the scalac sbt phase, this does not contain "X", I believe this is safe
    // TODO: report issue against sbt suggesting that they do the same
    val expectedNames = standardNames ++ Set("a", "A", "B", "C", "D", "b", "BB")
    usedNames === expectedNames
  }

  // test for https://github.com/gkossakowski/sbt/issues/5
  "symbolic names" in {
    val srcA = """|
			|class A {
			|  def `=`: Int = 3
			|}""".stripMargin
    val srcB = """|
			|class B {
			|  def foo(a: A) = a.`=`
			|}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(srcA, srcB)

    // DOTTY TODO: "Int" is not actually used, but we collect it because
    // it's the inferred return type so it appears in a TypeTree
    // We could avoid this by checking if the untyped tree has a return type
    // but is it worth it? Revisit this after https://github.com/sbt/sbt/issues/1104
    // has landed.
    val expectedNames = standardNames ++ Set("A", "a", "=", "Int")
    usedNames === expectedNames
  }

  "extract names in the types of trees" in {
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
    val src2 = """|object Test {
                  |  val x = B.lista
                  |  val y = B.at
                  |  val z = B.as
                  |  B.foo(???)
                  |  B.bar(???)
                  |}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(src1, src2)
    val expectedNames = standardNames ++ Set("Test", "Test$", "B", "B$",
      "Predef", "Predef$", "???", "Nothing",
      "lista", "List", "A",
      "at", "T", "X1", "X0",
      "as", "S", "Y",
      "foo", "M", "N",
      "bar", "P1", "P0")
    usedNames === expectedNames
  }

  // test for https://github.com/gkossakowski/sbt/issues/3
  "used names from the same compilation unit" in {
    val src = "class A { def foo: Int = 0; def bar: Int = foo }"
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(src)
    val expectedNames = standardNames ++ Set("A", "foo", "Int")
    usedNames === expectedNames
  }

  // pending test for https://issues.scala-lang.org/browse/SI-7173
  "names of constants" in {
    val src = "class A { final val foo = 12; def bar: Int = foo }"
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(src)
    val expectedNames = standardNames ++ Set("A", "foo", "Int")
    usedNames === expectedNames
  }

  // pending test for https://github.com/gkossakowski/sbt/issues/4
  // TODO: we should fix it by having special treatment of `selectDynamic` and `applyDynamic` calls
  "names from method calls on Dynamic" in {
    val srcA = """|import scala.language.dynamics
			|class A extends Dynamic {
			|	def selectDynamic(name: String): Int = name.length
			|}""".stripMargin
    val srcB = "class B { def foo(a: A): Int = a.bla }"
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = true)
    val usedNames = compilerForTesting.extractUsedNamesFromSrc(srcA, srcB)
    val expectedNames = standardNames ++ Set("B", "A", "a", "Int", "selectDynamic", "bla")
    usedNames === expectedNames
  }.pendingUntilFixed("Call to Dynamic is desugared in type checker so Select nodes is turned into string literal.")

}
