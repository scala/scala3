package dotty.tools.io

import scala.language.unsafeNulls

import org.junit.Test

import java.io.File
import dotty.tools.io.AbstractFile
import java.nio.file.{Files, Paths}
import java.nio.file.StandardCopyOption._
import java.nio.file.attribute.PosixFilePermissions
import dotty.tools.io.{ PlainDirectory, Directory, ClassPath }

class ClasspathTest {

  def pathsep = sys.props("path.separator")

  //
  // Cope with wildcard classpath entries, exercised with -classpath <cp>
  //
  // Verify that Windows users not forced to use backslash in classpath.
  //
  @Test def testWildcards(): Unit =
    val outDir = Files.createTempDirectory("classpath-test")
    try
      val compilerLib = "dist/target/pack/lib"
      val libdir = Paths.get(compilerLib).toFile
      if libdir.exists then
        val libjarFiles = libdir.listFiles.toList.take(5)
        try
          for src <- libjarFiles do
            val dest = Paths.get(s"$outDir/${src.getName}")
            printf("copy: %s\n", Files.copy(src.toPath, dest))

          val cp = Seq(s"$outDir/*", "not-a-real-directory/*").mkString(pathsep).replace('\\', '/')

          val libjars = libjarFiles.map { _.getName }.toSet

          // expand wildcard classpath entries, ignoring invalid entries
          val entries = ClassPath.expandPath(cp).map { Paths.get(_).toFile.getName }

          // require one-to-one matches
          assert(libjars == entries.toSet)

          printf("%d entries\n", entries.size)
          printf("%d libjars\n", libjars.size)

          for entry <- libjars do
            printf("libdir[%s]\n", entry)

          for entry <- entries do
            printf("expand[%s]\n", entry)

          // verify that expanded classpath has expected jar names
          for jar <- libjars do
            assert(entries.contains(jar))

        catch
          case _:NullPointerException => // no test if unable to copy jars to outDir


    finally
      deleteFile(outDir.toFile)


  private def deleteFile(target: File): Unit =
    if target.isDirectory then
      for member <- target.listFiles.toList
      do deleteFile(member)
    target.delete()
  end deleteFile
}
