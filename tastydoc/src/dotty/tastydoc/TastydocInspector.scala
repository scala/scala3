package dotty.tastydoc

import scala.quoted._
import scala.tasty.inspector.TastyInspector

import dotty.tastydoc.representations._

/* Extends TastyInspector and consumes Tasty Files to produce Representations
 *
 * @param mutablePackagesMap A mutable HashMap where seen packages are added
 */
class TastydocInspector(mutablePackagesMap: scala.collection.mutable.HashMap[String, EmulatedPackageRepresentation]) extends TastyInspector {

  protected def processCompilationUnit(using QuoteContext)(root: qctx.tasty.Tree): Unit = {
    representations.convertToRepresentation(root, None)(using mutablePackagesMap)
  }
}
