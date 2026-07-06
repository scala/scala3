package scala3build

import java.net.URL
import scala.concurrent.Promise
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Try
import scala.util.Using
import scala.util.Success
import scala.util.Failure
import mill.api.BuildCtx
import java.nio.file.Path

/**
 * Helper methods allowing to compile sources shared with the sbt build
 */
object sbt {
  private def root = BuildCtx.workspaceRoot

  type File = java.io.File

  def file(path: String): File =
    new File(path)

  extension (f: File) {
    private[sbt] def path: os.Path = os.Path(f, root)
    def /(str: String): File =
      (path / str.split('/').toSeq).toIO

    def relativeTo(other: File): Option[File] =
      Option.when(path.startsWith(other.path)) {
        path.subRelativeTo(other.path).toNIO.toFile
      }
  }

  extension (p: Path) {
    private[sbt] def path: os.Path = os.Path(p, root)
    def /(str: String): Path =
      (path / str.split('/').toSeq).toNIO
  }

  object IO {
    def copyFile(from: File, to: File): Unit =
      os.copy.over(from.path, to.path, createFolders = true)
    def copyDirectory(from: File, to: File): Unit =
      os.copy(from.path, to.path)
    def delete(f: File): Unit =
      os.remove.all(f.path)
    def read(f: File): String =
      os.read(f.path)
    def touch(f: File): Unit = {
      val path = f.path
      if (os.exists(path))
        os.mtime.set(path, System.currentTimeMillis())
      else
        os.write(path, Array.emptyByteArray, createFolders = true)
    }
    def write(f: File, content: os.Source): Unit =
      os.write.over(f.path, content)
    def readBytes(f: File): Array[Byte] =
      os.read.bytes(f.path)
    def temporaryDirectory: File =
      os.temp.dir().toIO
    def move(from: File, to: File): Unit =
      os.move(from.path, to.path, replaceExisting = true)
  }

  trait ProcessBuilderLike {
    def run(): ProcessLike
  }
  trait ProcessLike {
    def destroy(): Unit
    def exitValue(): Int
  }
  private final class URLProcessLike(url: URL, dest: os.Path) extends Thread(s"$url -> $dest") with ProcessLike {
    setDaemon(true)
    private val promise = Promise[Int]()
    override def run(): Unit =
      promise.success {
        val res = Try[Unit] {
          Using.resource(os.write.outputStream(dest)) { destOs =>
            Using.resource(url.openStream()) { sourceIs =>
              sourceIs.transferTo(destOs)
            }
          }
        }

        res match {
          case Success(()) => 0
          case Failure(ex) =>
            ex.printStackTrace(System.err)
            1
        }
      }
    def destroy(): Unit =
      interrupt()
    def exitValue(): Int =
      Await.result(promise.future, Duration.Inf)
  }
  private final class URLProcessBuilderLike(url: URL, dest: os.Path) extends ProcessBuilderLike {
    def run(): URLProcessLike = {
      val thread = new URLProcessLike(url, dest)
      thread.start()
      thread
    }
  }
  extension (url: URL) {
    def #>(dest: File): ProcessBuilderLike =
      new URLProcessBuilderLike(url, dest.path)
  }

  final class MessageOnlyException(override val toString: String) extends RuntimeException(toString)

  // only used in project/Dependencies.scala, where we only care about a few versions,
  // and disregard the sbt dependencies defined there
  extension(s: String) def %(other: String): String =
    ""
  extension(s: String) def %%(other: String): String =
    ""
}
