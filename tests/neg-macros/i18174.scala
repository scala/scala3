// does not compile anymore in Scala 3.4+
package pkg

import scala.language.`3.4`

trait P[+T]

extension [T](inline parse0: P[T])
  inline def | [V >: T](inline other: P[V]): P[V] = ???

extension [T](inline parse0: => P[T])
  inline def rep[V](inline min: Int = 0)(using repeater: Implicits.Repeater[T, V]): P[V] = ???

object Implicits:
  trait Repeater[-T, R]
  object Repeater:
    implicit def GenericRepeaterImplicit[T]: Repeater[T, Seq[T]] = ???

sealed trait RegexTree
abstract class Node extends RegexTree
class CharClassIntersection() extends Node

def classItem: P[RegexTree] = ???
def charClassIntersection: P[CharClassIntersection] = ???

def x =
  (charClassIntersection.rep() | classItem.rep()) // error
