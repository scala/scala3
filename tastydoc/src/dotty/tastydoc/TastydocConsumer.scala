package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

import dotty.tools.dotc.tastyreflect
import scala.annotation.tailrec

import java.io._

class TastydocConsumer extends TastyConsumer {
  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._

    val representationConversion = representations.convertToRepresentation(reflect)(root)
    val packagesSet = DocPrinter.traverseRepresentation(representationConversion, Set[(List[String], String)]())
    packagesSet.groupBy(x => x._1).foreach{ (k, m) =>
      if(k.nonEmpty){
        val file = new File("./" + DocPrinter.folderPrefix + k.mkString("/") + ".md")
        file.getParentFile.mkdirs
        val pw = new PrintWriter(file)
        pw.write(Md.header1("Package " + k.last))
        pw.write(Md.header2("Members:"))
        m.foreach(x => pw.write(x._2 + "\n\n"))
        pw.close
      }
    }
  }
}