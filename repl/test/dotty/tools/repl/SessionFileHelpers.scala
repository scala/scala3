package dotty.tools
package repl

import java.io.FileOutputStream
import java.nio.file.{Path, Files}
import java.util.jar.JarOutputStream

trait SessionFileHelpers:

  protected def tempFile(suffix: String = ".scala"): Path =
    val file = Files.createTempFile("repl_session", suffix)
    file.toFile.deleteOnExit()
    file

  protected def emptyJar(): Path =
    val jar = tempFile(".jar")
    new JarOutputStream(new FileOutputStream(jar.toFile)).close()
    jar

  protected def resourceDir(name: String, contents: String): Path =
    val dir = Files.createTempDirectory("repl_resources")
    dir.toFile.deleteOnExit()
    val file = dir.resolve(name)
    Files.writeString(file, contents)
    file.toFile.deleteOnExit()
    dir

  protected def contentOf(f: Path): String = Files.readString(f)

  protected val header: String = Save.sessionHeader
  protected val sep: String = Save.entrySeparator
