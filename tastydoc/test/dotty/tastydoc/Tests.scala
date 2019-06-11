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
  @Test def testListFromLib(): Unit = {
    Main.main(Array(
      "--classpath",
      "tastydoc/out/bootstrap/dotty-tastydoc-input/scala-0.16/classes",
      "--syntax",
      "wiki",
      "--packagestolink",
      "scala.*",
      "-i",
      "scala.collection.immutable.List"
    ))
  }
  @Test def testDotty(): Unit = {
    Main.main(Array(
      "--classpath",
      "tastydoc/dotty-0.15.0-RC1/lib",
      "--syntax",
      "wiki",
      "--packagestolink",
      "dotty.*",
      "scala.internal.*",
      "scala.annotation.*",
      "scala.implicits.*",
      "scala.compiletime.*",
      "scala.quoted.*",
      "scala.reflect.*",
      "scala.tasty.*",
      "scala.runtime.*",
      //Individual files
      "scala.Selectable*",
      "scala.NonEmptyTuple*",
      "scala.ValueOf*",
      //tbc
      "-d",
      "dotty",
      "scala"
    ))
  }
}