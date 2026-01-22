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

/** Tests for loading JARs that contain classes in scala.* packages.
  * This tests the fix for https://github.com/scala/scala3/issues/25058
  */
class ScalaPackageJarTests extends ReplTest:
  import ScalaPackageJarTests.*

  @Test def `i25058 load JAR with scala collection package classes` =
    // Skip if javac is not available
    Assume.assumeTrue("javac not available", ToolProvider.getSystemJavaCompiler != null)

    // Create a JAR with a class directly in scala.collection package
    // This tests the fix for #25058 where adding classes to existing scala.* packages
    // would fail because mergeNewEntries was skipping all scala.* packages
    val jarPath = createScalaCollectionJar()
    try
      initially {
        // First, access scala.collection to ensure it's loaded in the symbol table
        val state = run("scala.collection.mutable.ArrayBuffer.empty[Int]")
        storedOutput() // discard output
        state
      } andThen {
        // Load the JAR with a class in scala.collection
        val state = run(s":jar $jarPath")
        val output = storedOutput()
        assertTrue(s"Expected success message, got: $output", output.contains("Added") && output.contains("to classpath"))
        state
      } andThen {
        // Import from scala.collection - this would fail before the fix
        // because mergeNewEntries was skipping all scala.* packages
        val state = run("import scala.collection.TestCollection")
        val importOutput = storedOutput()
        // Should not have an error
        assertFalse(s"Import should succeed, got: $importOutput",
          importOutput.contains("Not Found Error") || importOutput.contains("not a member"))
        state
      } andThen {
        // Use the imported class
        run("TestCollection.getValue()")
        val output = storedOutput()
        assertTrue(s"Expected value 42, got: $output", output.contains("42"))
      }
    finally
      Files.deleteIfExists(Path.of(jarPath))

object ScalaPackageJarTests:

  /** Creates a JAR file containing a simple class directly in the scala.collection package.
    * This simulates what happens when a library adds classes to an existing scala.* package.
    *
    * The generated class is equivalent to:
    * {{{
    * package scala.collection;
    * public class TestCollection {
    *   public static int getValue() { return 42; }
    * }
    * }}}
    */
  def createScalaCollectionJar(): String =
    val tempDir = Files.createTempDirectory("scala-pkg-test")

    // Create package directory structure - using the existing scala.collection path
    val packageDir = tempDir.resolve("scala/collection")
    Files.createDirectories(packageDir)

    // Write Java source file
    val javaSource = packageDir.resolve("TestCollection.java")
    Files.writeString(javaSource,
      """|package scala.collection;
         |public class TestCollection {
         |  public static int getValue() { return 42; }
         |}
         |""".stripMargin)

    // Compile with javac
    val compiler = ToolProvider.getSystemJavaCompiler
    val fileManager = compiler.getStandardFileManager(null, null, null)
    val compilationUnits = fileManager.getJavaFileObjects(javaSource.toFile)
    val task = compiler.getTask(null, fileManager, null,
      java.util.Arrays.asList("-d", tempDir.toString), null, compilationUnits)
    val success = task.call()
    fileManager.close()

    if !success then
      throw new RuntimeException("Failed to compile test class")

    // Create JAR file
    val jarFile = tempDir.resolve("scala-collection-test.jar").toFile
    val manifest = new Manifest()
    manifest.getMainAttributes.putValue("Manifest-Version", "1.0")

    val jos = new JarOutputStream(new FileOutputStream(jarFile), manifest)
    try
      // Add the compiled class file
      val classFile = packageDir.resolve("TestCollection.class")
      jos.putNextEntry(new JarEntry("scala/collection/TestCollection.class"))
      jos.write(Files.readAllBytes(classFile))
      jos.closeEntry()
    finally
      jos.close()

    // Clean up source and class files (keep only the JAR)
    Files.walk(tempDir)
      .sorted(Comparator.reverseOrder[Path]())
      .filter(p => !p.equals(jarFile.toPath) && !p.equals(tempDir))
      .forEach(Files.delete)

    jarFile.getAbsolutePath
