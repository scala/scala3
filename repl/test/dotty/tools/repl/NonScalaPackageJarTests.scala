package dotty.tools
package repl

import scala.language.unsafeNulls

import java.io.{File, FileOutputStream}
import java.nio.file.{Files, Path}
import java.util.Comparator
import java.util.jar.{JarEntry, JarOutputStream, Manifest}
import javax.tools.ToolProvider

import org.junit.{Test, Assume}
import org.junit.Assert.{assertEquals, assertTrue, assertFalse}

/** Tests for loading JARs that contain classes in non-scala packages.
  * This tests the fix for cyclic reference issues when loading JARs
  * with overlapping packages (e.g., os-lib and os-lib-watch both defining the `os` package).
  */
class NonScalaPackageJarTests extends ReplTest:
  import NonScalaPackageJarTests.*

  @Test def `load JARs with overlapping non-scala packages` =
    // Skip if javac is not available
    Assume.assumeTrue("javac not available", ToolProvider.getSystemJavaCompiler != null)

    // Create two JARs that both define classes in the `testpkg` package
    // This simulates the case of os-lib and os-lib-watch both defining the `os` package
    val (jar1Path, jar2Path) = createOverlappingPackageJars()
    try
      initially {
        // Load the first JAR with testpkg.ClassA
        val state = run(s":jar $jar1Path")
        val output = storedOutput()
        assertTrue(s"Expected success message, got: $output", output.contains("Added") && output.contains("to classpath"))
        state
      } andThen {
        // Use ClassA to ensure the package is loaded
        val state = run("testpkg.ClassA.getValue()")
        val output = storedOutput()
        assertTrue(s"Expected value 1, got: $output", output.contains("1"))
        state
      } andThen {
        // Load the second JAR with testpkg.ClassB
        // This is where the cyclic reference error was happening
        val state = run(s":jar $jar2Path")
        val output = storedOutput()
        // Should succeed without cyclic reference error
        assertFalse(s"Should not have cyclic reference error, got: $output",
          output.contains("CyclicReference") || output.contains("Cyclic reference"))
        assertTrue(s"Expected success message, got: $output", output.contains("Added") && output.contains("to classpath"))
        state
      } andThen {
        // Use ClassB to ensure the second JAR was loaded correctly
        run("testpkg.ClassB.getValue()")
        val output = storedOutput()
        assertTrue(s"Expected value 2, got: $output", output.contains("2"))
      }
    finally
      Files.deleteIfExists(Path.of(jar1Path))
      Files.deleteIfExists(Path.of(jar2Path))

  @Test def `load JARs with nested overlapping packages` =
    // Skip if javac is not available
    Assume.assumeTrue("javac not available", ToolProvider.getSystemJavaCompiler != null)

    // Create two JARs where the second JAR adds both classes to an existing package
    // AND a new subpackage
    val (jar1Path, jar2Path) = createNestedOverlappingPackageJars()
    try
      initially {
        // Load the first JAR with testpkg.ClassA
        val state = run(s":jar $jar1Path")
        storedOutput() // discard output
        state
      } andThen {
        // Use ClassA to ensure the package is loaded
        val state = run("testpkg.ClassA.getValue()")
        storedOutput() // discard output
        state
      } andThen {
        // Load the second JAR with testpkg.ClassC AND testpkg.sub.ClassD
        val state = run(s":jar $jar2Path")
        val output = storedOutput()
        // Should succeed without cyclic reference error
        assertFalse(s"Should not have cyclic reference error, got: $output",
          output.contains("CyclicReference") || output.contains("Cyclic reference"))
        state
      } andThen {
        // Use ClassC to ensure the second JAR was loaded correctly
        val state = run("testpkg.ClassC.getValue()")
        val output = storedOutput()
        assertTrue(s"Expected value 3, got: $output", output.contains("3"))
        state
      } andThen {
        // Use ClassD from the subpackage
        run("testpkg.sub.ClassD.getValue()")
        val output = storedOutput()
        assertTrue(s"Expected value 4, got: $output", output.contains("4"))
      }
    finally
      Files.deleteIfExists(Path.of(jar1Path))
      Files.deleteIfExists(Path.of(jar2Path))

object NonScalaPackageJarTests:

  /** Creates two JARs that both define classes in the `testpkg` package.
    * JAR1 contains testpkg.ClassA
    * JAR2 contains testpkg.ClassB
    */
  def createOverlappingPackageJars(): (String, String) =
    val tempDir = Files.createTempDirectory("overlapping-pkg-test")

    // Create package directory
    val pkgDir = tempDir.resolve("testpkg")
    Files.createDirectories(pkgDir)

    // Write Java source for ClassA
    val classASource = pkgDir.resolve("ClassA.java")
    Files.writeString(classASource,
      """|package testpkg;
         |public class ClassA {
         |  public static int getValue() { return 1; }
         |}
         |""".stripMargin)

    // Write Java source for ClassB
    val classBSource = pkgDir.resolve("ClassB.java")
    Files.writeString(classBSource,
      """|package testpkg;
         |public class ClassB {
         |  public static int getValue() { return 2; }
         |}
         |""".stripMargin)

    // Compile with javac
    val compiler = ToolProvider.getSystemJavaCompiler
    val fileManager = compiler.getStandardFileManager(null, null, null)
    val compilationUnits = fileManager.getJavaFileObjects(classASource.toFile, classBSource.toFile)
    val task = compiler.getTask(null, fileManager, null,
      java.util.Arrays.asList("-d", tempDir.toString), null, compilationUnits)
    val success = task.call()
    fileManager.close()

    if !success then
      throw new RuntimeException("Failed to compile test classes")

    // Create JAR1 with ClassA
    val jar1File = tempDir.resolve("jar1.jar").toFile
    createJar(jar1File, tempDir, List("testpkg/ClassA.class"))

    // Create JAR2 with ClassB
    val jar2File = tempDir.resolve("jar2.jar").toFile
    createJar(jar2File, tempDir, List("testpkg/ClassB.class"))

    (jar1File.getAbsolutePath, jar2File.getAbsolutePath)

  /** Creates two JARs where JAR2 adds both classes to an existing package AND a new subpackage.
    * JAR1 contains testpkg.ClassA
    * JAR2 contains testpkg.ClassC and testpkg.sub.ClassD
    */
  def createNestedOverlappingPackageJars(): (String, String) =
    val tempDir = Files.createTempDirectory("nested-pkg-test")

    // Create package directories
    val pkgDir = tempDir.resolve("testpkg")
    val subPkgDir = tempDir.resolve("testpkg/sub")
    Files.createDirectories(subPkgDir)

    // Write Java source for ClassA
    val classASource = pkgDir.resolve("ClassA.java")
    Files.writeString(classASource,
      """|package testpkg;
         |public class ClassA {
         |  public static int getValue() { return 1; }
         |}
         |""".stripMargin)

    // Write Java source for ClassC
    val classCSource = pkgDir.resolve("ClassC.java")
    Files.writeString(classCSource,
      """|package testpkg;
         |public class ClassC {
         |  public static int getValue() { return 3; }
         |}
         |""".stripMargin)

    // Write Java source for ClassD in subpackage
    val classDSource = subPkgDir.resolve("ClassD.java")
    Files.writeString(classDSource,
      """|package testpkg.sub;
         |public class ClassD {
         |  public static int getValue() { return 4; }
         |}
         |""".stripMargin)

    // Compile with javac
    val compiler = ToolProvider.getSystemJavaCompiler
    val fileManager = compiler.getStandardFileManager(null, null, null)
    val compilationUnits = fileManager.getJavaFileObjects(
      classASource.toFile, classCSource.toFile, classDSource.toFile)
    val task = compiler.getTask(null, fileManager, null,
      java.util.Arrays.asList("-d", tempDir.toString), null, compilationUnits)
    val success = task.call()
    fileManager.close()

    if !success then
      throw new RuntimeException("Failed to compile test classes")

    // Create JAR1 with ClassA
    val jar1File = tempDir.resolve("jar1.jar").toFile
    createJar(jar1File, tempDir, List("testpkg/ClassA.class"))

    // Create JAR2 with ClassC and ClassD
    val jar2File = tempDir.resolve("jar2.jar").toFile
    createJar(jar2File, tempDir, List("testpkg/ClassC.class", "testpkg/sub/ClassD.class"))

    (jar1File.getAbsolutePath, jar2File.getAbsolutePath)

  private def createJar(jarFile: File, baseDir: Path, entries: List[String]): Unit =
    val manifest = new Manifest()
    manifest.getMainAttributes.putValue("Manifest-Version", "1.0")

    val jos = new JarOutputStream(new FileOutputStream(jarFile), manifest)
    try
      for entry <- entries do
        val path = baseDir.resolve(entry)
        jos.putNextEntry(new JarEntry(entry))
        jos.write(Files.readAllBytes(path))
        jos.closeEntry()
    finally
      jos.close()
