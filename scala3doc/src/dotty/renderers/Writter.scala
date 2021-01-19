package dotty.dokka
package renderers
import java.io.InputStream
import java.nio.file.Paths
import java.nio.file.Path
import java.nio.file.Files
import java.io.File

import HTML._

// TODO be more clever about writting - make it much faster!
trait Writter(using ctx: DocContext) extends Locations:
  private val args = summon[DocContext].args

  private def dest(path: String) =
    val absPath = args.output.toPath.resolve(path)
    if !Files.exists(absPath.getParent) then Files.createDirectories(absPath.getParent)
    absPath

  def write(dri: DRI, content: AppliedTag, ext: String = ".html"): String =
    val path = rawLocation(dri).mkString("", "/", ext)
    Files.write(dest(path), content.toString.getBytes)
    path

  def write(path: String, content: String): String =
    Files.write(dest(path), content.toString.getBytes)
    path

  def copy(from: Path, to: String): String =
    Files.copy(from, dest(to))
    to

  def copy(from: InputStream, to: String): String =
    Files.copy(from, dest(to))
    to