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
  // @Test def testDocumentation(): Unit = {
  //   ConsumeTasty(
  //     "tastydoc/out/bootstrap/dotty-tastydoc-input/scala-0.14/classes",
  //     List("example.level2.Documentation"),
  //     new dotty.tastydoc.TastydocConsumer
  //   )
  // }
  // @Test def testReturnTypeClass(): Unit = {
  //   ConsumeTasty(
  //     "tastydoc/out/bootstrap/dotty-tastydoc-input/scala-0.14/classes",
  //     List("example.ReturnTypeClass"),
  //     new dotty.tastydoc.TastydocConsumer
  //   )
  // }
  @Test def testListFromLib(): Unit = {
    ConsumeTasty(
      "tastydoc/out/bootstrap/dotty-tastydoc-input/scala-0.14/classes",
      List("scala.collection.immutable.List"),
      new dotty.tastydoc.TastydocConsumer
    )
  }
}