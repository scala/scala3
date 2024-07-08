sealed trait Schema[A]

object Schema extends RecordInstances:
  case class Field[A]()

sealed trait RecordInstances:
  self: Schema.type =>

  case class Record[A](field: Field[A]) extends Schema[A]

import Schema._

val field: Field[Int] = Field()

// Uh oh Found Playground.Schema.Field[Int] but Requried RecordInstances.this.Field[Int]
val record = Record[Int](field)