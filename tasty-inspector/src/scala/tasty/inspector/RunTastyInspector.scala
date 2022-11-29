package scala.tasty.inspector

import scala.quoted.*

import scala.io.Source

import java.io.File
import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.Paths
import java.text.Collator
import java.util.stream.Collectors
import scala.jdk.CollectionConverters._

import java.net.URLClassLoader


object StacktracesInspector extends Inspector:  
  override def inspect(using q: Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    import q.reflect.*

    for tasty <- tastys do
      println("AST:")
      println(tasty.ast.getClass)

def main(args: Array[String]): Unit = 
  val files: List[Path] = Files.walk(Paths.get("src/resources")).collect(Collectors.toList).asScala.toList.filter(_.toString.endsWith("tasty"))

  val (arr, rest) = files.partition(_.toString.contains("Array"))


  rest.foreach { f =>
    println("Class Name:")
    println(f.toString)
    TastyInspector.inspectTastyFiles(List(f.toAbsolutePath.toString))(StacktracesInspector)
    println()
  }


  println("How many Array.tasty files are there:")
  println(arr.size)
  println("How many Array.tasty files with different size are there: ")
  println(arr.map(f => Files.readAllBytes(f).toArray.size).distinct.size)

  arr.foreach { f =>
    println("Class Name:")
    println(f.toString)
    TastyInspector.inspectTastyFiles(List(f.toAbsolutePath.toString))(StacktracesInspector)
    println()
  }

