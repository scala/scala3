package dotty.semanticdb


import scala.tasty.Reflection
import scala.tasty.file._
import scala.collection.mutable.HashMap

import org.junit.Test
import org.junit.Assert._
import java.nio.file._
import scala.meta.internal.{semanticdb => s}
import scala.collection.JavaConverters._
import java.io.File
import scala.tasty.Reflection
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
  def recursiveListFiles(f: File): Array[File] = {
    val pattern = ".*\\.tasty".r
    val files = f.listFiles
    val folders = files.filter(_.isDirectory)
    val tastyfiles = files.filter(_.toString match {
      case pattern(x: _*) => true
      case _              => false
    })
    tastyfiles ++ folders.flatMap(recursiveListFiles)
  }

  /** Returns a mapping from *.scala file to a list of tasty files. */
  def getTastyFiles(classPath: Path): HashMap[String, List[Path]] = {
    val source_to_tasty: HashMap[String, List[Path]] = HashMap()
    val tastyfiles = recursiveListFiles(classPath.toFile())
    recursiveListFiles(classPath.toFile()).map(tasty_path => {
      val (classpath, classname) = getClasspathClassname(tasty_path.toPath())
      // We add an exception here to avoid crashing if we encountered
      // a bad tasty file
      try {
        val inspecter = new TastyScalaFileInferrer
        ConsumeTasty(classpath, classname :: Nil, inspecter)
        inspecter.sourcePath.foreach(
          source =>
            source_to_tasty +=
              (source -> (tasty_path
                .toPath() :: source_to_tasty.getOrElse(source, Nil))))
      } catch {
        case _: InvocationTargetException => println(tasty_path)
      }
    })
    source_to_tasty
  }

  /*
  Returns the list of names of class defined inside the scala file [scalaFile]
  extracted from the compilation artifacts found in [classPath].
  */
  def getClassNames(classPath: String, scalaFile: String): List[String] = {
    val tastyFiles =
      getTastyFiles(Paths.get(classPath).toAbsolutePath)
      .getOrElse(Paths.get(scalaFile).toAbsolutePath.toString, Nil)

    val tastyClasses = tastyFiles.map(getClasspathClassname)
    val (_, classnames) = tastyClasses.unzip
    return classnames
  }
}