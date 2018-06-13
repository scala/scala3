package scala.annotation.internal

import scala.annotation.RefiningAnnotation

/** An annotation used with AnnotatedTypes to capture a term-level expression's
 *  type precisely.
 *  @param tree A typed Tree.
 */
final class TypeOf(tree: Any) extends RefiningAnnotation
