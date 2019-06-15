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
  @Test def testDocumentation(): Unit = {
    Main.main(Array(
      "--classpath",
      "tastydoc/out/bootstrap/dotty-tastydoc-input/scala-0.16/classes",
      "--syntax",
      "wiki",
      //"markdown",
      "--packagestolink",
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
  // @Test def testDotty(): Unit = {
  //   Main.main(Array(
  //     "--classpath",
  //     "tastydoc/dotty-0.15.0-RC1/lib",
  //     "--syntax",
  //     "wiki",
  //     "--packagestolink",
  //     "dotty.*",
  //     "scala.annotation.*",
  //     "scala.compiletime.*",
  //     "scala.implicits.*",
  //     "scala.internal.*",
  //     "scala.quoted.*",
  //     "scala.reflect.*",
  //     "scala.runtime.*",
  //     "scala.tasty.*",
  //     "scala.testing.*",
  //     "scalaShadowing.*",
  //     //Individual files
  //     "scala.\\$times\\$colon*",
  //     "scala.Conversion*",
  //     "scala.Enum*",
  //     "scala.Eql*",
  //     "scala.forceInline*",
  //     "scala.FunctionXXL*",
  //     "scala.IArray*",
  //     "scala.NonEmptyTuple*",
  //     "scala.Product0*",
  //     "scala.Selectable*",
  //     "scala.Tuple*",
  //     "scala.TupleXXL*",
  //     "scala.ValueOf*",
  //     "-d",
  //     "dotty",
  //     "scala",
  //     "scalaShadowing"
  //   ))
  // }
}