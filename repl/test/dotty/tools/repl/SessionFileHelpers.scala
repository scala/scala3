package dotty.tools
package repl

import scala.language.unsafeNulls

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

  protected def contentOf(f: Path): String = Files.readString(f)

  protected val header: String = Save.sessionHeader
  protected val sep: String = Save.entrySeparator
