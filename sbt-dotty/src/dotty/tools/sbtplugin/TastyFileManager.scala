package dotty.tools.sbtplugin

import java.io.File
import java.nio.file.Files

import sbt.io.IO
import xsbti.compile.ClassFileManager

import scala.collection.mutable


/** A class file manger that prunes .tasty as needed.
 *
 *  This makes sure that, when a .class file must be deleted, the
 *  corresponding .tasty file is also deleted.
 *
 *  This code is adapted from Zinc `TransactionalClassFileManager`.
 *  We need to duplicate the logic since forwarding to the default class
 *  file manager doesn't work: we need to backup tasty files in a different
 *  temporary directory as class files.
 *
 *  To support older versions of dotty, this also takes care of .hasTasty
 *  files, although they are not used anymore.
 */
final class TastyFileManager extends ClassFileManager {
  private[this] var _tempDir: File = null
  private[this] def tempDir = {
    if (_tempDir == null) {
      _tempDir = Files.createTempDirectory("backup").toFile
    }
    _tempDir
  }

  private[this] val generatedTastyFiles = new mutable.HashSet[File]
  private[this] val movedTastyFiles = new mutable.HashMap[File, File]

  override def delete(classes: Array[File]): Unit = {
    val tasties = tastyFiles(classes)
    val toBeBackedUp = tasties
      .filter(t => t.exists && !movedTastyFiles.contains(t) && !generatedTastyFiles(t))
    for (c <- toBeBackedUp)
      movedTastyFiles.put(c, move(c))
    IO.deleteFilesEmptyDirs(tasties)
  }

  override def generated(classes: Array[File]): Unit =
    generatedTastyFiles ++= tastyFiles(classes)

  override def complete(success: Boolean): Unit = {
    if (!success) {
      IO.deleteFilesEmptyDirs(generatedTastyFiles)
      for ((orig, tmp) <- movedTastyFiles) IO.move(tmp, orig)
    }

    generatedTastyFiles.clear()
    movedTastyFiles.clear()
    if (_tempDir != null) {
      IO.delete(tempDir)
      _tempDir = null
    }
  }

  private def tastyFiles(classes: Array[File]): Array[File] = {
    val tastySuffixes = List(".tasty", ".hasTasty")
    classes.flatMap { classFile =>
      if (classFile.getPath.endsWith(".class")) {
        val prefix = classFile.getAbsolutePath.stripSuffix(".class")
        tastySuffixes.map(suffix => new File(prefix + suffix)).filter(_.exists)
      } else Nil
    }
  }

  private def move(c: File): File = {
    val target = File.createTempFile("sbt", ".tasty", tempDir)
    IO.move(c, target)
    target
  }
}
