package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

import dotty.tools.dotc.tastyreflect
import scala.annotation.tailrec

class TastydocConsumer extends TastyConsumer {

  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._

    val representationConversion = representations.convertToRepresentation(reflect)(root)
    val packagesSet = DocPrinter.traverseRepresentation(representationConversion, Set[(List[String], String)]())
    TastydocConsumer.mutablePackagesSet ++= packagesSet
  }
}

object TastydocConsumer {
  var userDocSyntax: String = "wiki"
  val mutablePackagesSet: scala.collection.mutable.HashSet[(List[String], String)] = new scala.collection.mutable.HashSet[(List[String], String)]
  var packagesToLink: List[String] = Nil
}