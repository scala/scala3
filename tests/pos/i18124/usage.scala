// usage.scala
package oolong.bson

extension (bv: BsonValue)
  def :+(other: BsonValue): BsonValue = merge(other, bv, false)

val x = foo
val y = inner.bar
