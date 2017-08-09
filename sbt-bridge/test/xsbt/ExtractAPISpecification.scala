/** Adapted from https://github.com/sbt/sbt/blob/0.13/compile/interface/src/test/scala/xsbt/ExtractAPISpecification.scala */
package xsbt

import org.junit.runner.RunWith
import xsbti.api._
import dotty.tools.dotc.sbt.DefaultShowAPI
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractAPISpecification extends Specification {

  "Existential types in method signatures" should {
    "have stable names" in { stableExistentialNames }
  }

  def stableExistentialNames: Boolean = {
    def compileAndGetFooMethodApi(src: String): Def = {
      val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = false)
      val sourceApi = compilerForTesting.extractApiFromSrc(src)
      val FooApi = sourceApi.definitions().find(_.name() == "Foo").get.asInstanceOf[ClassLike]
      val fooMethodApi = FooApi.structure().declared().find(_.name == "foo").get
      fooMethodApi.asInstanceOf[Def]
    }
    val src1 = """
				|class Box[T]
				|class Foo {
				|	def foo: Box[_] = null
				|
				}""".stripMargin
    val fooMethodApi1 = compileAndGetFooMethodApi(src1)
    val src2 = """
				|class Box[T]
				|class Foo {
			    |   def bar: Box[_] = null
				|	def foo: Box[_] = null
				|
				}""".stripMargin
    val fooMethodApi2 = compileAndGetFooMethodApi(src2)

    fooMethodApi1 == fooMethodApi2
    // Fails because xsbt.api is compiled with Scala 2.10
    // SameAPI.apply(fooMethodApi1, fooMethodApi2)
  }

  /**
   * Checks if representation of the inherited Namer class (with a declared self variable) in Global.Foo
   * is stable between compiling from source and unpickling. We compare extracted APIs of Global when Global
   * is compiled together with Namers or Namers is compiled first and then Global refers
   * to Namers by unpickling types from class files.
   *
   * See https://github.com/sbt/sbt/issues/2504
   */
  "Self variable and no self type" in {
    def selectNamer(api: SourceAPI): ClassLike = {
      def selectClass(defs: Iterable[Definition], name: String): ClassLike = defs.collectFirst {
        case cls: ClassLike if cls.name == name => cls
      }.get
      val global = selectClass(api.definitions, "Global")
      val foo = selectClass(global.structure.declared, "Global.Foo")
      selectClass(foo.structure.inherited, "Namers.Namer")
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
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = false)
    val apis = compilerForTesting.extractApisFromSrcs(reuseCompilerInstance = false)(List(src1, src2), List(src2))
    val _ :: src2Api1 :: src2Api2 :: Nil = apis.toList
    val namerApi1 = selectNamer(src2Api1)
    val namerApi2 = selectNamer(src2Api2)

    DefaultShowAPI(namerApi1) == DefaultShowAPI(namerApi2)
    // Fails because xsbt.api is compiled with Scala 2.10
    // SameAPI(namerApi1, namerApi2)
  }

  /**
   * Checks if self type is properly extracted in various cases of declaring a self type
   * with our without a self variable.
   */
  "Self type" in {
    def collectFirstClass(defs: Array[Definition]): ClassLike = defs.collectFirst {
      case c: ClassLike => c
    }.get
    val srcX = "trait X"
    val srcY = "trait Y"
    val srcC1 = "class C1 { this: C1 => }"
    val srcC2 = "class C2 { thisC: C2 => }"
    val srcC3 = "class C3 { this: X => }"
    val srcC4 = "class C4 { thisC: X => }"
    val srcC5 = "class C5 extends AnyRef with X with Y { self: X with Y => }"
    val srcC6 = "class C6 extends AnyRef with X { self: X with Y => }"
    // val srcC7 = "class C7 { _ => }" // DOTTY: Syntax not supported
    val srcC8 = "class C8 { self => }"
    val compilerForTesting = new ScalaCompilerForUnitTesting(nameHashing = false)
    val apis = compilerForTesting.extractApisFromSrcs(reuseCompilerInstance = true)(
      List(srcX, srcY, srcC1, srcC2, srcC3, srcC4, srcC5, srcC6, srcC8)
    ).map(x => collectFirstClass(x.definitions))
    val emptyType = new EmptyType
    def hasSelfType(c: ClassLike): Boolean =
      c.selfType != emptyType
    val (withSelfType, withoutSelfType) = apis.partition(hasSelfType)
    // DOTTY: In the scalac ExtractAPI phase, the self-type is only
    // extracted if it differs from the type of the class for stability
    // reasons. This isn't necessary in dotty because we always pickle
    // the self type.
    withSelfType.map(_.name).toSet === Set("C1", "C2", "C3", "C4", "C5", "C6", "C8")
    withoutSelfType.map(_.name).toSet === Set("X", "Y")
  }
}
