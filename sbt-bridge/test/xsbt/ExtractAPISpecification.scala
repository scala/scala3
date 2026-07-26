package xsbt

import xsbti.UseScope
import xsbti.api._
import xsbt.api.SameAPI

import org.junit.{ Test, Ignore }
import org.junit.Assert._

class ExtractAPISpecification {

  @Test
  def extractChildrenOfSealedClass = {
    def compileAndGetFooClassApi(src: String): ClassLike = {
      val compilerForTesting = new ScalaCompilerForUnitTesting
      val apis = compilerForTesting.extractApiFromSrc(src)
      val FooApi = apis.find(_.name() == "Foo").get
      FooApi
    }
    val src1 =
      """|sealed abstract class Foo
         |case class C1(x: Int) extends Foo
         |""".stripMargin
    val fooClassApi1 = compileAndGetFooClassApi(src1)
    val src2 =
      """|sealed abstract class Foo
         |case class C1(x: Int) extends Foo
         |case class C2(x: Int) extends Foo
         |""".stripMargin
    val fooClassApi2 = compileAndGetFooClassApi(src2)
    assertFalse(SameAPI(fooClassApi1, fooClassApi2))
  }

  @Ignore
  def extractDefinitionTypeOfPackageObject = {
    val src = "package object foo".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApiFromSrc(src)
    val Seq(fooClassApi) = apis.toSeq
    assertEquals(DefinitionType.PackageModule, fooClassApi.definitionType)
  }

  @Test
  def extractNestedClasses = {
    val src =
      """class A {
        |  class B
        |}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApiFromSrc(src).map(c => c.name -> c).toMap
    assertEquals(Set("A", "A.B"), apis.keys)
  }

  @Test
  def extractLocalClasses = {
    val src =
      """class A
        |class B
        |class C { def foo: Unit = { class Inner2 extends B } }
        |class D { def foo: Unit = { new B {} } }""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApiFromSrc(src).map(c => c.name -> c).toMap
    assertEquals(Set("A", "B", "C", "D"), apis.keys)
  }

  @Ignore
  def extractFlatApiOfNestedClass = {
    def compileAndGetFooClassApi(src: String): ClassLike = {
      val compilerForTesting = new ScalaCompilerForUnitTesting
      val apis = compilerForTesting.extractApiFromSrc(src)
      val FooApi = apis.find(_.name() == "Foo").get
      FooApi
    }
    val src1 =
      """class Foo {
        |  class A
        |}""".stripMargin
    val fooClassApi1 = compileAndGetFooClassApi(src1)
    val src2 =
      """class Foo {
        |  class A {
        |    def foo: Int = 123
        |  }
        |}""".stripMargin
    val fooClassApi2 = compileAndGetFooClassApi(src2)
    assertTrue(SameAPI(fooClassApi1, fooClassApi2))
  }

  @Test
  def extractPivateClasses = {
    val src =
      """private class A
        |class B { private class Inner1 extends A }
        |""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApiFromSrc(src).map(c => c.name -> c).toMap
    assertEquals(Set("A", "B", "B.Inner1"), apis.keys)
  }

  @Test
  def extractStableExistentialNames = {
    def compileAndGetFooMethodApi(src: String): Def = {
      val compilerForTesting = new ScalaCompilerForUnitTesting
      val sourceApi = compilerForTesting.extractApiFromSrc(src)
      val FooApi = sourceApi.find(_.name() == "Foo").get
      val fooMethodApi = FooApi.structure().declared().find(_.name == "foo").get
      fooMethodApi.asInstanceOf[Def]
    }
    val src1 = """
        |class Box[T]
        |class Foo {
        | def foo: Box[_] = null
        |
        }""".stripMargin
    val fooMethodApi1 = compileAndGetFooMethodApi(src1)
    val src2 = """
        |class Box[T]
        |class Foo {
          |   def bar: Box[_] = null
        | def foo: Box[_] = null
        |
        }""".stripMargin
    val fooMethodApi2 = compileAndGetFooMethodApi(src2)
    assertTrue("APIs are not the same.", SameAPI(fooMethodApi1, fooMethodApi2))
  }

  /**
   * Checks if representation of the inherited Namer class (with a declared self variable) in Global.Foo
   * is stable between compiling from source and unpickling. We compare extracted APIs of Global when Global
   * is compiled together with Namers or Namers is compiled first and then Global refers
   * to Namers by unpickling types from class files.
   */
  @Ignore
  def extractStableRepresentationOfSelfVariableThatHasNoSelfType = {
    def selectNamer(apis: Seq[ClassLike]): ClassLike = {
      // TODO: this doesn't work yet because inherited classes are not extracted
      apis.find(_.name == "Global.Foo.Namer").get
    }
    val src1 =
      """|class Namers {
        |  class Namer { thisNamer => }
        |}
        |""".stripMargin
    val src2 =
      """|class Global {
        |  class Foo extends Namers
        |}
        |""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis =
      compilerForTesting.extractApisFromSrcs(List(src1, src2), List(src2))
    val _ :: src2Api1 :: src2Api2 :: Nil = apis.toList: @unchecked 
    val namerApi1 = selectNamer(src2Api1)
    val namerApi2 = selectNamer(src2Api2)
    assertTrue(SameAPI(namerApi1, namerApi2))
  }

  @Ignore
  def extractDifferentRepresentationForAnInheritedClass = {
    val src =
      """|class A[T] {
         |  abstract class AA { def t: T }
         |}
         |class B extends A[Int]
      """.stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApiFromSrc(src).map(a => a.name -> a).toMap
    assertEquals(Set("A", "A.AA", "B", "B.AA"), apis.keySet)
    assertNotEquals(apis("A.AA"), apis("B.AA"))
  }

  // Tests for https://github.com/scala/scala3/issues/26231
  // Undercompilation when a case class field type changes and another file pattern-matches on it.

  /** Returns the declared members of the API for a named class/module in a given source. */
  private def declaredMembers(src: String, className: String, defType: DefinitionType): Array[ClassDefinition] = {
    val apis = new ScalaCompilerForUnitTesting().extractApiFromSrc(src)
    val cls = apis.find(c => c.name() == className && c.definitionType() == defType).get
    cls.structure().declared()
  }

  @Test
  def caseClassSelectorMethodApiChangesWhenFieldTypeChanges = {
    // `_1` is the product selector called at the bytecode level after pattern matching.
    // Its return type should change when the case class field type changes.
    val withNumber = declaredMembers(
      "case class Customer2(state: Number = java.lang.Integer.valueOf(1))",
      "Customer2", DefinitionType.ClassDef
    )
    val withString = declaredMembers(
      """case class Customer2(state: String = "")""",
      "Customer2", DefinitionType.ClassDef
    )

    val selector1WithNumber = withNumber.find(_.name() == "_1").get
    val selector1WithString = withString.find(_.name() == "_1").get

    assertFalse(
      "_1 API must differ between Number and String field type",
      SameAPI(selector1WithNumber, selector1WithString)
    )
  }

  @Test
  def companionUnapplyApiUnchangedWhenCaseClassFieldTypeChanges = {
    // In Scala 3, the compiler generates `def unapply(x: Customer2): Customer2 = x` for
    // case classes. Its signature is always `(Customer2): Customer2` regardless of field
    // types, so its API hash never changes when a field type changes.
    //
    // This is one half of the undercompilation bug: the dependent file (Test.scala) records
    // a used-name dependency on `unapply`, but `unapply`'s hash doesn't change, so Zinc
    // doesn't recompile Test.scala even though _1's return type changed.
    val unapplyWithNumber = declaredMembers(
      "case class Customer2(state: Number = java.lang.Integer.valueOf(1))",
      "Customer2", DefinitionType.Module
    ).find(_.name() == "unapply").get

    val unapplyWithString = declaredMembers(
      """case class Customer2(state: String = "")""",
      "Customer2", DefinitionType.Module
    ).find(_.name() == "unapply").get

    assertTrue(
      "unapply API is unchanged between Number and String field types (the stable signature that hides the change from Zinc)",
      SameAPI(unapplyWithNumber, unapplyWithString)
    )
  }

  @Test
  def caseClassPatternMatchRecordsConstructorAsUsedName = {
    // The other half of the undercompilation bug: when Test.scala does
    //   case Customer2(x) => x
    // the compiler emits a call to Customer2._1() in the bytecode. But
    // ExtractDependencies only records `unapply` as a used name (from the
    // UnApply tree node), never `_1`. Since `unapply`'s hash doesn't change
    // when field types change, Zinc won't recompile Test.scala.
    //
    // This test asserts the DESIRED (fixed) behaviour: the primary constructor
    // of Customer2 must appear in the used names so that any change to case
    // class parameters (type, arity, name) triggers recompilation.
    // Zinc mangles constructor names as `ClassName;init;`.
    val caseClassSrc =
      "case class Customer2(state: Number = java.lang.Integer.valueOf(1))"
    val patternMatchSrc =
      """|object Test {
         |  def test(c: Option[Customer2]): Any = c match {
         |    case Some(Customer2(x)) => x
         |    case None               => null
         |  }
         |}""".stripMargin

    // compileSrcs returns used names for all classes across all source files
    val output = new ScalaCompilerForUnitTesting()
      .compileSrcs(caseClassSrc, patternMatchSrc)
    val usedNames = output.analysis.usedNames

    // `unapply` is recorded because the typed UnApply tree references Customer2.unapply.
    assertTrue("unapply must be a used name in Test",
      usedNames("Test").contains("unapply"))

    // `unapplySeq` must also be recorded so that adding a unapplySeq alongside
    // an existing unapply (or vice versa) triggers recompilation.
    assertTrue("unapplySeq must be a used name in Test",
      usedNames("Test").contains("unapplySeq"))

    // The primary constructor must be recorded so that field type/arity/name
    // changes trigger recompilation.  Zinc uses the mangled name `Customer2;init;`.
    assertTrue("Customer2;init; must be a used name in Test",
      usedNames("Test").contains("Customer2;init;"))
  }

  @Test
  def handlePackageObjectsAndTypeCompanions = {
    val src =
      """|package object abc {
         |  type BuildInfoKey = BuildInfoKey.Entry[_]
         |  object BuildInfoKey {
         |    sealed trait Entry[A]
         |  }
         |}
      """.stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting.extractApiFromSrc(src).map(a => a.name -> a).toMap
    assertEquals(Set("abc.package", "abc.package$.BuildInfoKey", "abc.package$.BuildInfoKey$.Entry"), apis.keySet)
  }

  /**
   * Checks if self type is properly extracted in various cases of declaring a self type
   * with or without a self variable.
   */
  @Ignore
  def representASelfTypeCorrectly = {
    val srcX = "trait X"
    val srcY = "trait Y"
    val srcC1 = "class C1 { this: C1 => }"
    val srcC2 = "class C2 { thisC: C2 => }"
    val srcC3 = "class C3 { this: X => }"
    val srcC4 = "class C4 { thisC: X => }"
    val srcC5 = "class C5 extends AnyRef with X with Y { self: X with Y => }"
    val srcC6 = "class C6 extends AnyRef with X { self: X with Y => }"
    // val srcC7 = "class C7 { _ => }"
    val srcC8 = "class C8 { self => }"
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val apis = compilerForTesting
      .extractApisFromSrcs(
        List(srcX, srcY, srcC1, srcC2, srcC3, srcC4, srcC5, srcC6, srcC8)
      )
      .map(_.head)
    val emptyType = EmptyType.of()
    def hasSelfType(c: ClassLike): Boolean =
      c.selfType != emptyType
    val (withSelfType, withoutSelfType) = apis.partition(hasSelfType)
    assertEquals(Set("C3", "C4", "C5", "C6"), withSelfType.map(_.name).toSet)
    assertEquals(Set("X", "Y", "C1", "C2", "C8"), withoutSelfType.map(_.name).toSet)
  }
}
