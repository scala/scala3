// Trait.scala
package example

import quoted._

trait Trait {
  implicit val foo: Int = 23
}

object Trait {
  inline def get: Trait = ${ getImpl }

  def getImpl(using Quotes): Expr[Trait] = '{ new Trait {} }
}
