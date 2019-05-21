package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

import dotty.tools.dotc.tastyreflect
import scala.annotation.tailrec

import dotty.tastydoc.representations._

class TastydocConsumer(userDocSyntax: String, packagesToLink: List[String], mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackage]) extends TastyConsumer {

  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._

    val representationConversion = representations.convertToRepresentation(reflect)(root, None)
  }
}

object TastydocConsumer {
  var userDocSyntax: String = "wiki"
  val mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackage] = new scala.collection.mutable.HashMap()
  var packagesToLink: List[String] = Nil
}