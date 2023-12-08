//> using options -Xfatal-warnings

import scala.collection.mutable

object Test:
  type JsonPrimitive = String | Int | Double | Boolean | None.type

  type Rec[JA[_], JO[_], A] = A match
    case JsonPrimitive => JsonPrimitive | JA[Rec[JA, JO, JsonPrimitive]] | JO[Rec[JA, JO, JsonPrimitive]]
    case _ => A | JA[Rec[JA, JO, A]] | JO[Rec[JA, JO, A]]

  type Json = Rec[[A] =>> mutable.Buffer[A], [A] =>> mutable.Map[String, A], JsonPrimitive]

  type JsonObject = mutable.Map[String, Json]

  type JsonArray = mutable.Buffer[Json]

  def encode(x: Json): Int = x match
    case str: String => 1
    case b: Boolean => 2
    case i: Int => 3
    case d: Double => 4
    case arr: JsonArray => 5    // error
    case obj: JsonObject => 6   // error
    case _ => 7
