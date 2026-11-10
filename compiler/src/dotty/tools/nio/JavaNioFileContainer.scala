package dotty.tools.nio

import java.net.URI
import java.nio.file.{FileSystem, FileSystemNotFoundException, FileSystems, Files, Path, ProviderNotFoundException}
import scala.util.Properties

// This can reuse DiskDirectory because on the JVM its implementation already uses Java's NIO.
private object JavaNioFileContainer:
  def getJrt(path: String): Option[FileContainer] =
    try Some(new DiskDirectory(FileSystems.getFileSystem(URI.create("jrt:/")).getPath(path)))
    catch case _: ProviderNotFoundException | _: FileSystemNotFoundException => None

  def getCtSym(path: String): Option[FileContainer] =
    try
      val ctSym = Path.of(Properties.javaHome).resolve("lib").resolve("ct.sym")
      Option.when(Files.exists(ctSym))(new DiskDirectory(FileSystems.newFileSystem(ctSym, null: ClassLoader | Null).getPath("/")))
    catch case _: Exception => None