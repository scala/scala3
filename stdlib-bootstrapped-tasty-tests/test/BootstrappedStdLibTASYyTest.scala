package dotty.tools.dotc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._

import dotty.tools.io._
import dotty.tools.dotc.util.ClasspathFromClassloader

import scala.quoted._
import scala.tasty.inspector._

import java.io.File.pathSeparator
import java.io.File.separator

class BootstrappedStdLibTASYyTest:

  import BootstrappedStdLibTASYyTest._

  /** Test that we can load trees from TASTy */
  @Test def testTastyInspector: Unit =
    loadWithTastyInspector(loadBlacklisted)

  /** Test that we can load and compile trees from TASTy in a Jar */
  @Test def testFromTastyInJar: Unit =
    compileFromTastyInJar(loadBlacklisted.union(compileBlacklisted))

  /** Test that we can load and compile trees from TASTy */
  @Test def testFromTasty: Unit =
    compileFromTasty(loadBlacklisted.union(compileBlacklisted))

  @Test def blacklistNoDuplicates =
    def testDup(name: String, list: List[String], set: Set[String]) =
      assert(list.size == set.size,
        list.diff(set.toSeq).mkString(s"`$name` has duplicate entries:\n  ", "\n  ", "\n\n"))
    testDup("loadBlacklist", loadBlacklist, loadBlacklisted)
    testDup("compileBlacklist", compileBlacklist, compileBlacklisted)

  @Test def blacklistsNoIntersection =
    val intersection = loadBlacklisted & compileBlacklisted
    assert(intersection.isEmpty,
      intersection.mkString(
        "`compileBlacklist` contains names that are already in `loadBlacklist`: \n  ", "\n  ", "\n\n"))

  @Test def blacklistsOnlyContainsClassesThatExist =
    val scalaLibTastyPathsSet = scalaLibTastyPaths.toSet
    val intersection = loadBlacklisted & compileBlacklisted
    assert(loadBlacklisted.diff(scalaLibTastyPathsSet).isEmpty,
      loadBlacklisted.diff(scalaLibTastyPathsSet).mkString(
        "`loadBlacklisted` contains names that are not in `scalaLibTastyPaths`: \n  ", "\n  ", "\n\n"))
    assert(compileBlacklisted.diff(scalaLibTastyPathsSet).isEmpty,
      compileBlacklisted.diff(scalaLibTastyPathsSet).mkString(
        "`loadBlacklisted` contains names that are not in `scalaLibTastyPaths`: \n  ", "\n  ", "\n\n"))

  @Ignore
  @Test def testLoadBacklistIsMinimal =
    var shouldBeWhitelisted = List.empty[String]
    val size = loadBlacklisted.size
    for (notBlacklisted, i) <- loadBlacklist.zipWithIndex do
      val blacklist = loadBlacklisted - notBlacklisted
      println(s"Trying withouth $notBlacklisted in the blacklist  (${i+1}/$size)")
      try {
        loadWithTastyInspector(blacklist)
        shouldBeWhitelisted = notBlacklisted :: shouldBeWhitelisted
      }
      catch {
        case ex: Throwable => // ok
      }
    assert(shouldBeWhitelisted.isEmpty,
      shouldBeWhitelisted.mkString("Some classes do not need to be blacklisted in `loadBlacklisted`\n  ", "\n  ", "\n\n"))

  @Ignore
  @Test def testCompileBlacklistIsMinimal =
    var shouldBeWhitelisted = List.empty[String]
    val size = compileBlacklisted.size
    val blacklist0 = loadBlacklisted.union(compileBlacklisted)
    for (notBlacklisted, i) <- compileBlacklist.zipWithIndex do
      val blacklist = blacklist0 - notBlacklisted
      println(s"Trying withouth $notBlacklisted in the blacklist (${i+1}/$size)")
      try {
        compileFromTastyInJar(blacklist)
        shouldBeWhitelisted = notBlacklisted :: shouldBeWhitelisted
      }
      catch {
        case ex: Throwable => // ok
      }
    assert(shouldBeWhitelisted.isEmpty,
      shouldBeWhitelisted.mkString("Some classes do not need to be blacklisted in `compileBlacklisted`\n  ", "\n  ", "\n\n"))

end BootstrappedStdLibTASYyTest

object BootstrappedStdLibTASYyTest:

  def scalaLibJarPath = System.getProperty("dotty.scala.library")
  def scalaLibClassesPath =
    java.nio.file.Paths.get(scalaLibJarPath).getParent.resolve("classes").normalize

  val scalaLibTastyPaths =
    new Directory(scalaLibClassesPath).deepFiles
      .filter(_.`extension` == "tasty")
      .map(_.normalize.path.stripPrefix(scalaLibClassesPath.toString + "/"))
      .toList

  def loadWithTastyInspector(blacklisted: Set[String]): Unit =
    val inspector = new scala.tasty.inspector.Inspector {
      def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
        for tasty <- tastys do
          tasty.ast.show(using quotes.reflect.Printer.TreeStructure) // Check that we can traverse the full tree
        ()
    }
    val tastyFiles = scalaLibTastyPaths.filterNot(blacklisted)
    val isSuccess = TastyInspector.inspectTastyFiles(tastyFiles.map(x => scalaLibClassesPath.resolve(x).toString))(inspector)
    assert(isSuccess, "Errors reported while loading from TASTy")

  def compileFromTastyInJar(blacklisted: Set[String]): Unit = {
    val driver = new dotty.tools.dotc.Driver
    val yFromTastyBlacklist =
      blacklisted.mkString("-Yfrom-tasty-ignore-list:", ",", "")
    val args = Array(
      "-classpath", ClasspathFromClassloader(getClass.getClassLoader),
      "-from-tasty",
      "-nowarn",
      yFromTastyBlacklist,
      scalaLibJarPath,
    )
    val reporter = driver.process(args)
    assert(reporter.errorCount == 0, "Errors while re-compiling")
  }

  def compileFromTasty(blacklisted: Set[String]): Unit = {
    val driver = new dotty.tools.dotc.Driver
    val tastyFiles = scalaLibTastyPaths.filterNot(blacklisted)
    val args = Array(
      "-classpath", ClasspathFromClassloader(getClass.getClassLoader),
      "-from-tasty",
      "-nowarn",
    ) ++ tastyFiles.map(x => scalaLibClassesPath.resolve(x).toString)
    val reporter = driver.process(args)
    assert(reporter.errorCount == 0, "Errors while re-compiling")
  }

  /** List of tasty files that cannot be loaded from TASTy */
  def loadBlacklist = List[String](
    // No issues :)
  )

  /** List of tasty files that cannot be recompilied from TASTy */
  def compileBlacklist = List[String](
    // See #10048
    // failed: java.lang.AssertionError: assertion failed: class Boolean
    //   at dotty.tools.backend.jvm.BCodeHelpers$BCInnerClassGen.assertClassNotArrayNotPrimitive(BCodeHelpers.scala:247)
    //   at dotty.tools.backend.jvm.BCodeHelpers$BCInnerClassGen.getClassBTypeAndRegisterInnerClass(BCodeHelpers.scala:265)
    //   at dotty.tools.backend.jvm.BCodeHelpers$BCInnerClassGen.getClassBTypeAndRegisterInnerClass$(BCodeHelpers.scala:210)
    //   at dotty.tools.backend.jvm.BCodeSkelBuilder$PlainSkelBuilder.getClassBTypeAndRegisterInnerClass(BCodeSkelBuilder.scala:62)
    //   at dotty.tools.backend.jvm.BCodeHelpers$BCInnerClassGen.internalName(BCodeHelpers.scala:237)
    "scala/Array.tasty",
    "scala/Boolean.tasty",
    "scala/Byte.tasty",
    "scala/Char.tasty",
    "scala/Double.tasty",
    "scala/Float.tasty",
    "scala/Int.tasty",
    "scala/Long.tasty",
    "scala/Short.tasty",
    "scala/Unit.tasty",
  ).map(_.replace("/", separator))

  /** Set of tasty files that cannot be loaded from TASTy */
  def loadBlacklisted = loadBlacklist.toSet

  /** Set of tasty files that cannot be recompilied from TASTy */
  def compileBlacklisted = compileBlacklist.toSet

end BootstrappedStdLibTASYyTest
