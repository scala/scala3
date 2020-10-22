package example

import scala.quoted._

trait Trait {
  final val foo = 23
}

object Trait {
  inline def get: Trait = ${ getImpl }

  def getImpl(using Quotes): Expr[Trait] = '{ new Trait {} }
}
