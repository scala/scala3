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

  @Test def `i25058 load JAR with scala collection parallel subpackage` =
    // Skip if javac is not available
    Assume.assumeTrue("javac not available", ToolProvider.getSystemJavaCompiler != null)

    // Create a JAR that simulates scala-parallel-collections:
    // - Classes in scala.collection (e.g., ParIterable)
    // - Classes in scala.collection.parallel subpackage (e.g., CollectionConverters)
    // The bug was that when a package has BOTH classes AND subpackages,
    // only the classes were processed and subpackages were skipped.
    val jarPath = createScalaCollectionParallelJar()
    try
      initially {
        // First, access scala.collection to ensure it's loaded in the symbol table
        val state = run("scala.collection.mutable.ArrayBuffer.empty[Int]")
        storedOutput() // discard output
        state
      } andThen {
        // Load the JAR with classes in scala.collection AND scala.collection.parallel
        val state = run(s":jar $jarPath")
        val output = storedOutput()
        assertTrue(s"Expected success message, got: $output", output.contains("Added") && output.contains("to classpath"))
        state
      } andThen {
        // Import from scala.collection.parallel - this is the key test for #25058
        val state = run("import scala.collection.parallel.TestParallel")
        val importOutput = storedOutput()
        // Should not have an error
        assertFalse(s"Import should succeed, got: $importOutput",
          importOutput.contains("Not Found Error") || importOutput.contains("not a member"))
        state
      } andThen {
        // Use the imported class
        run("TestParallel.getValue()")
        val output = storedOutput()
        assertTrue(s"Expected value 42, got: $output", output.contains("42"))
      }
    finally
      Files.deleteIfExists(Path.of(jarPath))

object ScalaPackageJarTests:

  /** Creates a JAR file simulating scala-parallel-collections structure:
    * - A class in scala.collection (TestParIterable)
    * - A class in scala.collection.parallel (TestParallel)
    *
    * This is critical for testing #25058: the bug only manifests when
    * a JAR adds BOTH classes to an existing package (scala.collection)
    * AND a new subpackage (scala.collection.parallel).
    */
  def createScalaCollectionParallelJar(): String =
    val tempDir = Files.createTempDirectory("scala-pkg-test")

    // Create package directory structures
    val collectionDir = tempDir.resolve("scala/collection")
    val parallelDir = tempDir.resolve("scala/collection/parallel")
    Files.createDirectories(parallelDir)

    // Write Java source file in scala.collection (simulates ParIterable etc.)
    val collectionSource = collectionDir.resolve("TestParIterable.java")
    Files.writeString(collectionSource,
      """|package scala.collection;
         |public class TestParIterable {
         |  public static int getCount() { return 100; }
         |}
         |""".stripMargin)

    // Write Java source file in scala.collection.parallel (simulates CollectionConverters etc.)
    val parallelSource = parallelDir.resolve("TestParallel.java")
    Files.writeString(parallelSource,
      """|package scala.collection.parallel;
         |public class TestParallel {
         |  public static int getValue() { return 42; }
         |}
         |""".stripMargin)

    // Compile with javac
    val compiler = ToolProvider.getSystemJavaCompiler
    val fileManager = compiler.getStandardFileManager(null, null, null)
    val compilationUnits = fileManager.getJavaFileObjects(collectionSource.toFile, parallelSource.toFile)
    val task = compiler.getTask(null, fileManager, null,
      java.util.Arrays.asList("-d", tempDir.toString), null, compilationUnits)
    val success = task.call()
    fileManager.close()

    if !success then
      throw new RuntimeException("Failed to compile test classes")

    // Create JAR file
    val jarFile = tempDir.resolve("scala-collection-parallel.jar").toFile
    val manifest = new Manifest()
    manifest.getMainAttributes.putValue("Manifest-Version", "1.0")

    val jos = new JarOutputStream(new FileOutputStream(jarFile), manifest)
    try
      // Add class in scala.collection
      val collectionClass = collectionDir.resolve("TestParIterable.class")
      jos.putNextEntry(new JarEntry("scala/collection/TestParIterable.class"))
      jos.write(Files.readAllBytes(collectionClass))
      jos.closeEntry()

      // Add class in scala.collection.parallel
      val parallelClass = parallelDir.resolve("TestParallel.class")
      jos.putNextEntry(new JarEntry("scala/collection/parallel/TestParallel.class"))
      jos.write(Files.readAllBytes(parallelClass))
      jos.closeEntry()
    finally
      jos.close()

    // Clean up source and class files (keep only the JAR)
    Files.walk(tempDir)
      .sorted(Comparator.reverseOrder[Path]())
      .filter(p => !p.equals(jarFile.toPath) && !p.equals(tempDir))
      .forEach(Files.delete)

    jarFile.getAbsolutePath
