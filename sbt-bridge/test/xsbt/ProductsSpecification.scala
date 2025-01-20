package xsbt

import org.junit.Assert.*
import org.junit.Ignore
import org.junit.Test

import java.io.File
import java.nio.file.Path
import java.nio.file.Paths

class ProductsSpecification {

  @Test
  def extractProductsFromJar = {
    val src =
      """package example
        |
        |class A {
        |  class B
        |  def foo =
        |    class C
        |}""".stripMargin
    val output = compiler.compileSrcsToJar(src)
    val srcFile = output.srcFiles.head
    val products = output.analysis.productClassesToSources.filter(_._2 == srcFile).keys.toSet

    def toPathInJar(className: String): Path =
      Paths.get(s"${output.classesOutput}!${className.replace('.', File.separatorChar)}.class")
    val expected = Set("example.A", "example.A$B", "example.A$C$1").map(toPathInJar)
    assertEquals(products, expected)
  }

  @Test
  def extractNonLocalClassesNoInc = {
    val src =
      """package example
        |
        |class A {
        |  class B
        |  def foo =
        |    class C
        |}""".stripMargin
    val output = compiler.compileSrcsNoInc(src)
    val srcFile = output.srcFiles.head
    val (srcNames, binaryNames) = output.analysis.classNames(srcFile).unzip // non local class names

    assertFalse(output.analysis.enabled()) // inc phases are disabled
    assertTrue(output.analysis.apis.isEmpty) // extract-api did not run
    assertTrue(output.analysis.usedNamesAndScopes.isEmpty) // extract-dependencies did not run

    // note that local class C is not included, classNames only records non local classes
    val expectedBinary = Set("example.A", "example.A$B")
    assertEquals(expectedBinary, binaryNames.toSet)

    // note that local class C is not included, classNames only records non local classes
    val expectedSrc = Set("example.A", "example.A.B")
    assertEquals(expectedSrc, srcNames.toSet)
  }

  private def compiler = new ScalaCompilerForUnitTesting
}
