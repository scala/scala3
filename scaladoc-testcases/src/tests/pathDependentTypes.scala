package tests
package pathDependentTypes

import deriving.Mirror.ProductOf

// issue 16143

trait Foo[A]:
  type Out

trait Bar[A]:
  type Out

def foo[A](using f: Foo[A])(using b: Bar[f.Out]): b.Out
  = ???

// issue 16057

def fromProductTyped[P <: Product](p: P)(using m: ProductOf[P]): m.MirroredElemTypes
  = ???
