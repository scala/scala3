package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

import dotty.tools.dotc.tastyreflect
import scala.annotation.tailrec

import dotty.tastydoc.representations._

class TastydocConsumer(mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation]) extends TastyConsumer {

  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._

    val representationConversion = representations.convertToRepresentation(reflect)(root, None) given (mutablePackagesMap)
  }
}