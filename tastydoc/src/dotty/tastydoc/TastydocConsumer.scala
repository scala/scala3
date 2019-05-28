package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer

import dotty.tools.dotc.tastyreflect
import scala.annotation.tailrec

import dotty.tastydoc.representations._

/* Extends TastyConsumer and consumes Tasty Files to produce Representations
 *
 * @param mutablePackagesMap A mutable HashMap where seen packages are added
 */
class TastydocConsumer(mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation]) extends TastyConsumer {

  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._

    representations.convertToRepresentation(reflect)(root, None) given (mutablePackagesMap)
  }
}