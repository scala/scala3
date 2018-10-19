package xsbt

import xsbti.TestCallback.ExtractedClassDependencies

import org.junit.Test
import org.junit.Assert._

class DependencySpecification {

  @Test
  def extractedClassDependenciesFromPublicMembers = {
    val classDependencies = extractClassDependenciesPublic
    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    assertEquals(Set.empty, memberRef("A"))
    assertEquals(Set.empty, inheritance("A"))
    assertEquals(Set("A", "D"), memberRef("B"))
    assertEquals(Set("D"), inheritance("B"))
    assertEquals(Set("A"), memberRef("C"))
    assertEquals(Set.empty, inheritance("C"))
    assertEquals(Set.empty, memberRef("D"))
    assertEquals(Set.empty, inheritance("D"))
    assertEquals(Set.empty, memberRef("E"))
    assertEquals(Set.empty, inheritance("E"))
    assertEquals(Set("A", "B", "D", "E", "G", "C"), memberRef("F")) // C is the underlying type of MyC
    assertEquals(Set("A", "E"), inheritance("F"))
    assertEquals(Set("B", "E", "G"), memberRef("H"))
    // aliases and applied type constructors are expanded so we have inheritance dependency on B
    assertEquals(Set("B", "E"), inheritance("H"))
  }

  @Test
  def extractedClassDependenciesFromLocalMembers = {
    val classDependencies = extractClassDependenciesLocal
    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    val localInheritance = classDependencies.localInheritance
    assertEquals(Set.empty, memberRef("A"))
    assertEquals(Set.empty, inheritance("A"))
    assertEquals(Set.empty, memberRef("B"))
    assertEquals(Set.empty, inheritance("B"))
    assertEquals(Set("A"), memberRef("C.Inner1"))
    assertEquals(Set("A"), inheritance("C.Inner1"))
    assertEquals(Set("B"), memberRef("D"))
    assertEquals(Set.empty, inheritance("D"))
    assertEquals(Set("B"), localInheritance("D"))
    assertEquals(Set("B"), memberRef("E"))
    assertEquals(Set.empty, inheritance("E"))
    assertEquals(Set("B"), localInheritance("E"))
  }

  @Test
  def extractedClassDependenciesWithTraitAsFirstParent = {
    val classDependencies = extractClassDependenciesTraitAsFirstPatent
    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    assertEquals(Set.empty, memberRef("A"))
    assertEquals(Set.empty, inheritance("A"))
    assertEquals(Set("A"), memberRef("B"))
    assertEquals(Set("A"), inheritance("B"))
    // verify that memberRef captures the oddity described in documentation of `Relations.inheritance`
    // we are mainly interested whether dependency on A is captured in `memberRef` relation so
    // the invariant that says that memberRef is superset of inheritance relation is preserved
    assertEquals(Set("A", "B"), memberRef("C"))
    assertEquals(Set("A", "B"), inheritance("C"))
    // same as above but indirect (C -> B -> A), note that only A is visible here
    assertEquals(Set("A", "C"), memberRef("D"))
    assertEquals(Set("A", "C"), inheritance("D"))
  }

  @Test
  def extractedClassDependenciesFromARefinement = {
    val srcFoo =
      "object Outer {\n  class Inner { type Xyz }\n\n  type TypeInner = Inner { type Xyz = Int }\n}"
    val srcBar = "object Bar {\n  def bar: Outer.TypeInner = null\n}"

    val compilerForTesting = new ScalaCompilerForUnitTesting
    val classDependencies =
      compilerForTesting.extractDependenciesFromSrcs(srcFoo, srcBar)

    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    assertEquals(Set.empty, memberRef("Outer"))
    assertEquals(Set.empty, inheritance("Outer"))
    assertEquals(Set("Outer", "Outer$.Inner"), memberRef("Bar"))
    assertEquals(Set.empty, inheritance("Bar"))
  }

  @Test
  def extractedClassDependenciesOnAnObjectCorrectly = {
    val srcA =
      """object A {
        |   def foo = { B; () }
        |}""".stripMargin
    val srcB = """object B { println("foo") }"""

    val compilerForTesting = new ScalaCompilerForUnitTesting
    val classDependencies =
      compilerForTesting.extractDependenciesFromSrcs(srcA, srcB)

    val memberRef = classDependencies.memberRef
    val inheritance = classDependencies.inheritance
    assertEquals(Set("B"), memberRef("A"))
    assertEquals(Set.empty, inheritance("A"))
    assertEquals(Set.empty, memberRef("B"))
    assertEquals(Set.empty, inheritance("B"))
  }

  @Test
  def extractedTopLevelImportDependencies = {
    val srcA =
      """
        |package abc
        |object A {
        |  class Inner
        |}
        |class A2""".stripMargin
    val srcB = "import abc.A; import abc.A.Inner; class B"
    val srcC = "import abc.{A, A2}; class C"
    val srcD = "import abc.{A2 => Foo}; class D"
    val srcE = "import abc.A._; class E"
    val srcF = "import abc._; class F"
    val srcG =
      """|package foo {
         |  package bar {
         |    import abc.A
         |    class G
         |  }
         |}
      """.stripMargin
    val srcH = "class H { import abc.A }"

    val compilerForTesting = new ScalaCompilerForUnitTesting
    val deps = compilerForTesting
      .extractDependenciesFromSrcs(srcA, srcB, srcC, srcD, srcE, srcF, srcG, srcH)
      .memberRef

    assertEquals(Set.empty, deps("A"))
    assertEquals(Set("abc.A", "abc.A$.Inner"), deps("B"))
    assertEquals(Set("abc.A", "abc.A2"), deps("C"))
    assertEquals(Set("abc.A2"), deps("D"))
    assertEquals(Set("abc.A"), deps("E"))
    assertEquals(Set.empty, deps("F"))
    assertEquals(Set("abc.A"), deps("foo.bar.G"))
    assertEquals(Set("abc.A"), deps("H"))
  }

  @Test
  def extractedClassDependenciesOnPackageObject = {
    val srcA = "package object foo { def bar = 1 }"
    val srcB =
      """|package foo
         |
         |class Test {
         |  bar
         |}
      """.stripMargin

    val compilerForTesting = new ScalaCompilerForUnitTesting
    val classDependencies =
      compilerForTesting.extractDependenciesFromSrcs(srcA, srcB)

    val memberRef = classDependencies.memberRef
    assertEquals(Set("foo.package"), memberRef("foo.Test"))
  }

  private def extractClassDependenciesPublic: ExtractedClassDependencies = {
    val srcA = "class A"
    val srcB = "class B extends D[A]"
    val srcC = """|class C {
      |  def a: A = null
      |}""".stripMargin
    val srcD = "class D[T]"
    val srcE = "trait E[T]"
    val srcF = "trait F extends A with E[D[B]] { self: G.MyC => }"
    val srcG = "object G { type T[x] = B ; type MyC = C }"
    // T is a type constructor [x]B
    // B extends D
    // E verifies the core type gets pulled out
    val srcH = "trait H extends G.T[Int] with (E[Int] @unchecked)"

    val compilerForTesting = new ScalaCompilerForUnitTesting
    val classDependencies =
      compilerForTesting.extractDependenciesFromSrcs(srcA, srcB, srcC, srcD, srcE, srcF, srcG, srcH)
    classDependencies
  }

  private def extractClassDependenciesLocal: ExtractedClassDependencies = {
    val srcA = "class A"
    val srcB = "class B"
    val srcC = "class C { private class Inner1 extends A }"
    val srcD = "class D { def foo: Unit = { class Inner2 extends B } }"
    val srcE = "class E { def foo: Unit = { new B {} } }"

    val compilerForTesting = new ScalaCompilerForUnitTesting
    val classDependencies =
      compilerForTesting.extractDependenciesFromSrcs(srcA, srcB, srcC, srcD, srcE)
    classDependencies
  }

  private def extractClassDependenciesTraitAsFirstPatent: ExtractedClassDependencies = {
    val srcA = "class A"
    val srcB = "trait B extends A"
    val srcC = "trait C extends B"
    val srcD = "class D extends C"

    val compilerForTesting = new ScalaCompilerForUnitTesting
    val classDependencies =
      compilerForTesting.extractDependenciesFromSrcs(srcA, srcB, srcC, srcD)
    classDependencies
  }
}