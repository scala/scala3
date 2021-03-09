package dotty.tools.io

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
    import dotty.tools.io.ClassPath
    val outDir = Files.createTempDirectory("classpath-test")
    try
      val compilerLib = "dist/target/pack/lib"
      val libdir = Paths.get(compilerLib).toFile
      if libdir.exists then
        try for src <- libdir.listFiles.toList.take(5) do
          val dest = Paths.get(s"$outDir/${src.getName}")
          printf("copy: %s\n",Files.copy(src.toPath,dest)) // ,REPLACE_EXISTING,COPY_ATTRIBUTES))
        catch
          case _:NullPointerException => // ignore errors adding jars to outDir

      //outDir.toFile.listFiles.toList.foreach { printf("%s\n",_) }
      val cp = Seq(s"$compilerLib/*",s"$outDir/*","not-a-real-directory/*").mkString(pathsep).replace('\\','/')
     
      // need to expand wildcard classpath entries
      val entries = ClassPath.expandPath(cp)
      for entry <- entries.take(10) do
        println(entry)

    finally
      deleteFile(outDir.toFile)


  private def deleteFile(target: File): Unit =
    if target.isDirectory then
      for member <- target.listFiles.toList
      do deleteFile(member)
    target.delete()
  end deleteFile
}
