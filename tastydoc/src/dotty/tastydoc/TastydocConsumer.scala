package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

import dotty.tools.dotc.tastyreflect
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.annotation.tailrec

import java.io._

class TastydocConsumer extends TastyConsumer {
  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._
    import internal._
    import references._

    println("Full tree =========================")
    println(root.show)
    println("End of full tree ==================")

    println("Start convert to Representation")

    //convertToEntity(root)
    //println(formatRepresentationToMarkdown(representations.convertToRepresentation(reflect)(root), false))

    val pw = new PrintWriter(new File("./tastydoc/docOutputTest.md" ))
    pw.write(formatRepresentationToMarkdown(representations.convertToRepresentation(reflect)(root), false))
    pw.close()

  }
}