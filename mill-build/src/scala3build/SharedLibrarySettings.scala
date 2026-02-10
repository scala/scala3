package scala3build

import mill.*
import mill.scalalib.*

import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*
import scala.util.Using

trait SharedLibrarySettings extends CrossScala3Module {

  def isJS: Boolean = false

  // Original comment: TODO: Enable fatal warnings after 3.8 because old stdlib has different nullability.
  def scalacOptions = super.scalacOptions() ++ Seq(
    "-Yexplicit-nulls"
  )

  def patches: T[Seq[PathRef]]

  def compile = Task {
    val res = super.compile()
    val updatedClassDir = Task.dest
    for (elem <- os.list(res.classes.path))
      os.copy(elem, updatedClassDir / elem.last)

    val keepExtensions = if (isJS) Seq(".class", ".sjsir") else Seq(".class")

    for (jarOrDir <- patches().map(_.path)) {
      def process(relPath: os.SubPath, content: => Array[Byte]): Unit = {
        val shortName = keepExtensions.foldLeft(relPath.toString)(_.stripSuffix(_))
        val dest = updatedClassDir / relPath
        val shouldCopy = LibraryFilesToCopy.set.contains(shortName) || LibraryFilesToCopy.set.exists(n => shortName.startsWith(n + "$"))
        if (shouldCopy) {
          System.err.println(s"Overwriting $relPath")
          StripScala2Annotations.patchFile(updatedClassDir, relPath, content)
        }
        else if (!os.exists(dest)) {
          System.err.println(s"Copying $relPath")
          StripScala2Annotations.patchFile(updatedClassDir, relPath, content)
        }
      }

      System.err.println(s"Copying entries from $jarOrDir")

      if (os.isDir(jarOrDir))
        os.walk(jarOrDir)
          .filter(f => keepExtensions.exists(f.last.endsWith))
          .filter(os.isFile)
          .sortBy(_.subRelativeTo(jarOrDir).toString)
          .foreach { f =>
            process(f.subRelativeTo(jarOrDir), os.read.bytes(f))
          }
      else
        Using.resource(new ZipFile(jarOrDir.toIO)) { zf =>
          for (ent <- zf.entries().asScala if !ent.isDirectory && keepExtensions.exists(ent.getName.endsWith))
            process(os.sub / ent.getName.split('/').toSeq, zf.getInputStream(ent).readAllBytes())
        }
    }

    mill.javalib.api.CompilationResult(res.analysisFile, PathRef(updatedClassDir))
  }
}
