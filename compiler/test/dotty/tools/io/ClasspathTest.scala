package dotty.tools.io

import org.junit.Test

import dotty.tools.io.ClassPath
import dotty.tools.nio.*

class ClasspathTest {

  def pathsep: String = sys.props("path.separator").nn

  def isWindows: Boolean = scala.util.Properties.isWin

  //
  // Cope with wildcard classpath entries, exercised with -classpath <cp>
  //
  // Verify that Windows users not forced to use backslash in classpath.
  //
  @Test def testWildcards(): Unit =
    val outDir = FileContainer.createTemporaryOnDisk("classpath-test")
    try
      val compilerLib = s"${if isWindows then "dist-win-x86_64" else "dist"}/target/pack/lib"
      FileContainer.getOnDisk(compilerLib) match
        case Some(libdir) =>
          val libjarFiles = libdir.entries.collect{ case f: File => f }.toList.take(5)
          for src <- libjarFiles do
            val dest = outDir.getOrCreateFile(src.name)
            src.copyTo(dest)
            printf("copy: %s\n", dest)

          val cp = Seq(s"$outDir/*", "not-a-real-directory/*").mkString(pathsep).replace('\\', '/')

          val libjars = libjarFiles.map { _.name }.toSet

          // expand wildcard classpath entries, ignoring invalid entries
          val entries = ClassPath.expandPath(cp).map { File.getOnDisk(_).get.name }

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

        case None => ()
    finally
      outDir.deleteRecursively()

}
