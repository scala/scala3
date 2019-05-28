package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.file._
import scala.collection.mutable.HashMap

import org.junit.Test
import org.junit.Assert._
import java.nio.file._
import scala.collection.JavaConverters._
import java.io.File
import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer
import java.lang.reflect.InvocationTargetException

class Tests {
  // @Test def testAccess(): Unit = {
  //   ConsumeTasty(
  //     "tastydoc/out/bootstrap/dotty-tastydoc-input/scala-0.14/classes",
  //     List("example.Access"),
  //     new dotty.tastydoc.TastydocConsumer
  //   )
  // }
  // @Test def testExample(): Unit = {
  //   ConsumeTasty(
  //     "tastydoc/out/bootstrap/dotty-tastydoc-input/scala-0.14/classes",
  //     List("example.Example"),
  //     new dotty.tastydoc.TastydocConsumer
  //   )
  // }
  // @Test def testExample2(): Unit = {
  //   ConsumeTasty(
  //     "tastydoc/out/bootstrap/dotty-tastydoc-input/scala-0.14/classes",
  //     List("example.OExample"),
  //     new dotty.tastydoc.TastydocConsumer
  //   )
  // }
  // @Test def testClasses(): Unit = {
  //   ConsumeTasty(
  //     "tastydoc/out/bootstrap/dotty-tastydoc-input/scala-0.14/classes",
  //     List("example.C2"),
  //     new dotty.tastydoc.TastydocConsumer
  //   )
  // }
  // @Test def testMethods(): Unit = {
  //   ConsumeTasty(
  //     "tastydoc/out/bootstrap/dotty-tastydoc-input/scala-0.14/classes",
  //     List("example.Methods"),
  //     new dotty.tastydoc.TastydocConsumer
  //   )
  // }
  @Test def testDocumentation(): Unit = {
    Main.main(Array(
      "-classpath",
      "tastydoc/out/bootstrap/dotty-tastydoc-input/scala-0.16/classes",
      "-syntax",
      "wiki",
      "-packagestolink",
      "example.*",
      //"scala.*",
      "-i",
      "example.level2.Documentation",
      "example.level2.ClassExtendingDocumentation",
      "example.DocumentationInheritance",
      "example.ReturnTypeClass",
      "example.level2.SameLevelTypeLinking",
      "example.ReturnObjectWithType",
      "example.level2.TraitWithCompanion",
      "example.level2.level3.level4.ClassLevel4",
      "example.UserDocLinkingClass"
    ))
  }
  @Test def testListFromLib(): Unit = {
    Main.main(Array(
      "-classpath",
      "tastydoc/out/bootstrap/dotty-tastydoc-input/scala-0.16/classes",
      "-syntax",
      "wiki",
      "-packagestolink",
      "scala.*",
      "-i",
      "scala.collection.immutable.List"
    ))
  }
}