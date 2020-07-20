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


object Main:
  def main(args: Array[String]): Unit =
    val cp = args.headOption.getOrElse("/home/krzysiek/workspace/dotty-dokka/target/scala-0.25/classes")
    def listTastyFiles(f: File): Seq[File] = 
      val (files, dirs) = f.listFiles().partition(_.isFile)
      files.filter(_.getName.endsWith(".tasty")) ++ dirs.flatMap(listTastyFiles)
    
    val tastyFiles = cp.split(File.pathSeparatorChar).toList.flatMap(p => listTastyFiles(new File(p))).map(_.toString)

    val inspector = new DokkaTastyInspector()
    inspector.inspect(System.getProperty("java.class.path"), tastyFiles)

    val config = DottyDokkaConfig(inspector.result())
    new DokkaGenerator(config, DokkaConsoleLogger.INSTANCE).generate()
    println("Done")
