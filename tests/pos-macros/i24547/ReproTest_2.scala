// ReproTest.scala
// Test case: abstract type parameter Coll[Int] with @retains annotation
// causes "not a legal path" error when macro creates new ValDef

import scala.collection.IterableOps

def reproTest[Coll[X] <: Iterable[X] & IterableOps[X, Coll, Coll[X]]]: Unit =
    def xsValues: Coll[Int] = ???

    // The .span method returns (Coll[Int], Coll[Int])
    // With capture checking, these types get @retains annotations
    // When the macro creates new ValDefs with these types, compilation fails
    ReproMacro.transform {
      val (take, drop) = xsValues.span(???)
      take.toSeq
    }
