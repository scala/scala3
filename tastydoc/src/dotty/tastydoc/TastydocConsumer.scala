package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

import dotty.tools.dotc.tastyreflect
import scala.annotation.tailrec

import java.io._

class TastydocConsumer extends TastyConsumer {
  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._

    // println("Full tree =========================")
    // println(root.show)
    // println("End of full tree ==================")

    println("Start convert to Representation")

    //convertToEntity(root)
    //println(formatRepresentationToMarkdown(representations.convertToRepresentation(reflect)(root), false))

    val representationConversion = representations.convertToRepresentation(reflect)(root)
    DocPrinter.traverseRepresentation(representationConversion, Set[(String, String)]())

  }
}