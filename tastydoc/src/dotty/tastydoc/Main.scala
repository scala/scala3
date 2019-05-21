package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.file._

import java.io._

object Main {
  def main(args: Array[String]): Unit = {
    TastydocConsumer.userDocSyntax = {
      val idx = args.indexOf("-syntax")
      if(idx >= 0 && args.size > idx + 1){
        if(args(idx + 1) == "markdown"){
          "markdown"
        }else if(args(idx + 1) == "wiki"){
          "wiki"
        }else{
          println("Wrong parameter for -syntax. Using default: wiki")
          "wiki"
        }
      }else{
        "wiki"
      }
    }

    TastydocConsumer.packagesToLink = {
      val idx = args.indexOf("-packagestolink")
      if(idx >= 0 && args.size > idx + 1){
        args.drop(idx + 1).takeWhile(! _.startsWith("-")).toList
      }else{
        Nil
      }
    }


    val extraClasspath = {
      val idx = args.indexOf("-classpath")
      if(idx >= 0 && args.size > idx + 1){
        args(idx + 1)
      }else{
        "."
      }
    }

    val classes = {
      val idx = args.indexOf("-i")
      if(idx >= 0 && args.size > idx + 1){
        args.drop(idx + 1).toList
      }else{
        Nil
      }
    }

    if (classes.isEmpty) {
      println("Dotty Tastydoc: No classes were passed as argument")
    } else {
      println("Running Dotty Tastydoc on: " + classes.mkString(" "))
      val x = new TastydocConsumer(null, null, null)
      ConsumeTasty(extraClasspath, classes, x)

      TastydocConsumer.mutablePackagesMap.foreach{(k, v) =>
        DocPrinter.traverseRepresentation(v)
        val file = new File("./" + DocPrinter.folderPrefix + k.replaceAll("\\.", "/") + "/" + v.name + ".md")
        file.getParentFile.mkdirs
        val pw = new PrintWriter(file)
        pw.write(DocPrinter.formatRepresentationToMarkdown(v, k.split("\\.").toList))
        pw.close
      }

      // TastydocConsumer.mutablePackagesSet.groupBy(x => x._1).foreach{ (k, m) =>
      //   if(k.nonEmpty){
      //     val file = new File("./" + DocPrinter.folderPrefix + k.mkString("/") + ".md")
      //     file.getParentFile.mkdirs
      //     val pw = new PrintWriter(file)
      //     pw.write(Md.header1("Package " + k.last))
      //     pw.write(Md.header2("Members:"))
      //     m.foreach(x => pw.write(x._2 + "\n\n"))
      //     pw.close
      //   }
      // }
    }
  }
}
