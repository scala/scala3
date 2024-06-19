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

  private def compiler = new ScalaCompilerForUnitTesting
}