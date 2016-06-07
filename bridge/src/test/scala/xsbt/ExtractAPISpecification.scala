/** Adapted from https://github.com/sbt/sbt/blob/0.13/compile/interface/src/test/scala/xsbt/ExtractAPISpecification.scala */
package xsbt

import org.junit.runner.RunWith
import xsbti.api.ClassLike
import xsbti.api.Def
import xsbt.api.ShowAPI
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractAPISpecification extends Specification {

  "Existential types in method signatures" should {
    "have stable names" in { stableExistentialNames }
  }

  def stableExistentialNames: Boolean = {
    def compileAndGetFooMethodApi(src: String): Def = {
      val compilerForTesting = new ScalaCompilerForUnitTesting
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
}
