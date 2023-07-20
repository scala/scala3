// while making MT post-redux consistent in its normalisation/simplification
// one version of the change broke a line of the perspective community project in CI
// this is a minimisation of the failure

import scala.compiletime._, scala.deriving._

transparent inline def foo(using m: Mirror): Unit =
  constValueTuple[m.MirroredElemLabels].toList.toSet // was:
//-- [E057] Type Mismatch Error: cat.scala:8:14 ----------------------------------
//8 |def test = foo(using summon[Mirror.Of[Cat]])
//  |           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//  |Type argument ("name" : String) | Nothing does not conform to lower bound m$proxy1.MirroredElemLabels match {
//  |  case EmptyTuple => Nothing
//  |  case h *: t => h | Tuple.Fold[t, Nothing, [x, y] =>> x | y]
//  |}
//  |-----------------------------------------------------------------------------
//  |Inline stack trace
//  |- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//  |This location contains code that was inlined from cat.scala:4
//4 |  constValueTuple[m.MirroredElemLabels].toList.toSet
//  |                                               ^

case class Cat(name: String)

def test = foo(using summon[Mirror.Of[Cat]])
