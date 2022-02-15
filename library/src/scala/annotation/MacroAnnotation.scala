// TODO in which package should this class be located?
package scala
package annotation

import scala.quoted._
import scala.annotation.{StaticAnnotation, experimental}

// @experimental
trait MacroAnnotation extends StaticAnnotation {
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    quotes.reflect.report.errorAndAbort(tree.show, tree.pos)
  def transformObject(using Quotes)(valTree: quotes.reflect.ValDef, classTree: quotes.reflect.TypeDef): List[quotes.reflect.Definition] =
    quotes.reflect.report.errorAndAbort(classTree.show, classTree.pos)
  def transformParam(using Quotes)(paramTree: quotes.reflect.Definition, ownerTree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    quotes.reflect.report.errorAndAbort(paramTree.show, paramTree.pos)
}
