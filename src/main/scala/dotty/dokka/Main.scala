package dotty.dokka

import org.jetbrains.dokka._
import org.jetbrains.dokka.utilities._
import org.jetbrains.dokka.plugability._
import java.util.ServiceLoader
import java.io.File
import java.util.jar._
import collection.JavaConverters._

import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector
import dotty.tastydoc.representations
import dotty.tastydoc.representations._

case class DocConfiguration(tastyFiles: List[String], classpath: String)

object Main:
  def main(args: Array[String]): Unit =
    // TODO #20 change the default to something more reasonable...
    val cp = args.headOption.getOrElse("target/scala-0.25/classes")
    def listTastyFiles(f: File): Seq[String] = 
      val (files, dirs) = f.listFiles().partition(_.isFile)
      files.filter(_.getName.endsWith(".tasty")).map(_.toString) ++ dirs.flatMap(listTastyFiles)
    
    val config = DocConfiguration(
      tastyFiles = 
        for
          root <- cp.split(File.pathSeparatorChar).toList
          tastyFile <- listTastyFiles(new File(root)) if tastyFile.contains("tests") || tastyFile.contains("example")
        yield tastyFile,
      classpath = System.getProperty("java.class.path")
    )

    // TODO #20 pass options, classpath etc.
    new DokkaGenerator(new DottyDokkaConfig(config), DokkaConsoleLogger.INSTANCE).generate()
    println("Done")
