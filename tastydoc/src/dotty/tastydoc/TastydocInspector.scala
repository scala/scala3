package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector

import dotty.tastydoc.representations._

/* Extends TastyInspector and consumes Tasty Files to produce Representations
 *
 * @param mutablePackagesMap A mutable HashMap where seen packages are added
 */
class TastydocInspector(mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation]) extends TastyInspector {

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._

    representations.convertToRepresentation(reflect)(root, None)(given mutablePackagesMap)
  }
}
