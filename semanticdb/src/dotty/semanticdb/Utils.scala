package dotty.semanticdb


import scala.tasty.file._
import scala.collection.mutable.HashMap

import org.junit.Test
import org.junit.Assert._
import java.nio.file._
import scala.meta.internal.{semanticdb => s}
import scala.collection.JavaConverters._
import java.io.File
import scala.tasty.file.TastyConsumer
import java.lang.reflect.InvocationTargetException

object Utils {
  /** Infers a tuple (class path, class name) from a given path */
  def getClasspathClassname(file: Path): (String, String) = {
    val pat = """(.*)\..*""".r
    val classpath = file.getParent().getParent().toString()
    val modulename = file.getParent().getFileName().toString()
    val sourcename =
      file.toFile().getName().toString() match {
        case pat(name) => name
        case _         => ""
      }
    return (classpath, modulename + "." + sourcename)
  }

  /** List all tasty files occuring in the folder f or one of its subfolders */
  def recursiveListFiles(f: File, prefix : String = ""): Array[File] = {
    val pattern = (".*" + prefix + ".*\\.tasty").r
    val files = f.listFiles
    val folders = files.filter(_.isDirectory)
    val tastyfiles = files.filter(_.toPath.toString match {
      case pattern(x: _*) => true
      case _              => false
    })
    tastyfiles ++ folders.flatMap(recursiveListFiles(_, prefix))
  }

  /** Returns a mapping from *.scala file to a list of tasty files. */
  def getTastyFiles(classPath: Path, prefix : String = ""): HashMap[String, List[Path]] = {
    val sourceToTasty: HashMap[String, List[Path]] = HashMap()
    val tastyfiles = recursiveListFiles(classPath.toFile(), prefix)
    tastyfiles.map(tastyPath => {
      val (classpath, classname) = getClasspathClassname(tastyPath.toPath())
      // We add an exception here to avoid crashing if we encountered
      // a bad tasty file
      try {
        val inspecter = new TastyScalaFileInferrer
        ConsumeTasty(classpath, classname :: Nil, inspecter)
        inspecter.sourcePath.foreach(
          source =>
            sourceToTasty +=
              (source -> (tastyPath
                .toPath().toAbsolutePath :: sourceToTasty.getOrElse(source, Nil))))
      } catch {
        case _: InvocationTargetException => ()
      }
    })
    sourceToTasty
  }

  /*
  Returns the list of names of class defined inside the scala file [scalaFile]
  extracted from the compilation artifacts found in [classPath].
  */
  def getClassNames(classPath: Path, scalaFile: Path, prefix : String = ""): List[String] = {
    val tastyFiles = getTastyFiles(classPath.toAbsolutePath, prefix)
    getClassNamesCached(scalaFile, tastyFiles)
  }

  def getClassNamesCached(scalaFile: Path, allFiles : HashMap[String, List[Path]]): List[String] = {
    val tastyFiles =
      allFiles
      .getOrElse(scalaFile.toString, Nil)

    val tastyClasses = tastyFiles.map(getClasspathClassname)
    val (_, classnames) = tastyClasses.unzip
    return classnames
  }
}