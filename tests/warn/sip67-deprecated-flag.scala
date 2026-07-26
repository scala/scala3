//> using options -deprecation -language:experimental.strictEqualityPatternMatching

import scala.language.strictEquality

sealed trait Foo[A]
object Foo:
  object Bar extends Foo[Int]

def a[A](a: Foo[A]) =
  a match
    case Foo.Bar =>

// nopos-warn `strictEqualityPatternMatching` is now standard, no language import is needed
