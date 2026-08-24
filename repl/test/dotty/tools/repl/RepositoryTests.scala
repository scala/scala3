package dotty.tools
package repl

import scala.language.unsafeNulls

import java.io.FileOutputStream
import java.nio.file.{Files, Path}
import java.util.jar.JarOutputStream

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

class RepositoryTests extends ReplTest:

  private val organization = "dotty.repl.test"
  private val artifact = "tiny"
  private val version = "1.0.0"
  private val dependency = s"$organization:$artifact:$version"

  private lazy val localRepositoryDirectory: Path =
    val repository = Files.createTempDirectory("repl_repository")
    val directory = organization.split('.').foldLeft(repository)(_.resolve(_)).resolve(artifact).resolve(version)
    Files.createDirectories(directory)
    Files.writeString(directory.resolve(s"$artifact-$version.pom"),
      s"""<project>
         |  <modelVersion>4.0.0</modelVersion>
         |  <groupId>$organization</groupId>
         |  <artifactId>$artifact</artifactId>
         |  <version>$version</version>
         |</project>""".stripMargin)
    new JarOutputStream(new FileOutputStream(directory.resolve(s"$artifact-$version.jar").toFile)).close()
    repository

  private lazy val localRepository: String = localRepositoryDirectory.toUri.toString

  @Test def `repository parses aliases, URLs, ivy patterns and local directories`: Unit =
    List("central", "ivy2Local", "ivy2local", "m2Local", "m2local", "https://jitpack.io",
         "file:///tmp/repo", "ivy:file:///tmp/repo/[defaultPattern]",
         "ivy:file:///tmp/repo/[defaultPattern]|file:///tmp/repo/[module]/ivy-[revision].xml",
         localRepositoryDirectory.toString).foreach: spec =>
      assertTrue(spec, DependencyResolver.parseRepository(spec).isDefined)

  @Test def `repository command accepts a local directory without a file URL`: Unit =
    initially {
      val stateAfterRepository = run(s":repository $localRepositoryDirectory")
      assertEquals(s"Added repository '$localRepositoryDirectory'.", storedOutput().trim)
      stateAfterRepository
    } andThen {
      run(s":dep $dependency")
      assertEquals("Resolved a dependency (1 JARs)", storedOutput().trim)
    }

  @Test def `repository command reports what it cannot parse`: Unit =
    initially:
      run(":repository bogusrepo")
      assertEquals("Unable to parse repository 'bogusrepo'.", storedOutput().trim)

  @Test def `repository command without a value prints its usage`: Unit =
    initially:
      run(":repository")
      assertEquals(":repository <url>|<alias> ...", storedOutput().trim)

  @Test def `repository directive applies to dependencies of the same block`: Unit =
    initially:
      run(s"//> using dep $dependency\n//> using repository $localRepository")
      assertEquals(
        s"""Added repository '$localRepository'.
           |Resolved a dependency (1 JARs)""".stripMargin,
        storedOutput().trim
      )
