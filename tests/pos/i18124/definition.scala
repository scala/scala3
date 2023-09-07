// definition.scala
package oolong.bson:

  trait BsonValue
  protected def merge(
      base: BsonValue,
      patch: BsonValue,
      arraySubvalues: Boolean = false
  ): BsonValue = ???

  private def foo: Int = 1

  package inner:
    protected[bson] def bar = 2

