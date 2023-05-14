package dotty.tools.io

import scala.language.unsafeNulls

import org.junit.Test

import dotty.tools.io.AbstractFile
import java.nio.file.Files._
import java.nio.file.attribute.PosixFilePermissions

class AbstractFileTest {
  //
  // Cope with symbolic links. Exercised by -d output.
  //
  // BytecodeWriters#getFile ensures d.isDirectory for elements in path,
  // but d.fileNamed and d.subdirectoryNamed also Files.createDirectories
  // for prefixes, which may optionally fail on an existing symbolic link.
  //
  private def exerciseSymbolicLinks(temp: Directory): Unit = {
    val base = {
      val permissions = PosixFilePermissions.fromString("rwxrwxrwx")
      val attributes = PosixFilePermissions.asFileAttribute(permissions)
      // Specifying the 'attributes' parameter on Windows prevents a
      // FileSystemException "A required privilege is not held by the client".
      val target = createTempDirectory(temp.jpath, "real", attributes)
      val link   = temp.jpath.resolve("link")
      createSymbolicLink(link, target)     // may bail early if unsupported
      AbstractFile.getDirectory(link)
    }

    val file = base.fileNamed("foo")
    assert(file.exists && !file.isDirectory)

    val dir = base.subdirectoryNamed("bar")
    assert(dir.isDirectory)
    val leaf = dir.fileNamed("basement")
    assert(leaf.exists && !leaf.isDirectory)

    val nested = AbstractFile.getDirectory(createSymbolicLink(dir.jpath.resolve("link"), dir.subdirectoryNamed("nested").jpath))
    val doubly = nested.fileNamed("doubly")
    assert(nested.exists && nested.isDirectory)  // /tmp/link/bar/link
    assert(doubly.exists && !doubly.isDirectory) // /tmp/link/bar/link/doubly
    assert(dir.subdirectoryNamed("link").exists)
  }
  @Test def t6450(): Unit =
    try Directory.inTempDirectory(exerciseSymbolicLinks)
    catch { case _: UnsupportedOperationException => () }
}

/* Was:
[error] Test dotty.tools.io.AbstractFileTest.t6450 failed: java.nio.file.FileAlreadyExistsException: /var/folders/2_/xb149z895wb5f1y632xp2bw40000gq/T/temp9110019868196066936/link, took 0.124 sec
[error]     at sun.nio.fs.UnixException.translateToIOException(UnixException.java:88)
[error]     at sun.nio.fs.UnixException.rethrowAsIOException(UnixException.java:102)
[error]     at sun.nio.fs.UnixException.rethrowAsIOException(UnixException.java:107)
[error]     at sun.nio.fs.UnixFileSystemProvider.createDirectory(UnixFileSystemProvider.java:384)
[error]     at java.nio.file.Files.createDirectory(Files.java:674)
[error]     at java.nio.file.Files.createAndCheckIsDirectory(Files.java:781)
[error]     at java.nio.file.Files.createDirectories(Files.java:727)
[error]     at dotty.tools.io.AbstractFile.fileOrSubdirectoryNamed(AbstractFile.scala:242)
[error]     at dotty.tools.io.AbstractFile.fileNamed(AbstractFile.scala:262)
[error]     at dotty.tools.io.AbstractFileTest.exerciseSymbolicLinks(AbstractFileTest.scala:25)
 */
