import scala.language.strictEquality

class Foo(i: Int) extends AnyVal

val _ = summon[CanEqual[Nothing, Nothing]]

val _ = summon[CanEqual[Int, Nothing]]
val _ = summon[CanEqual[Nothing, Int]]
val _ = summon[CanEqual[3, Nothing]]
val _ = summon[CanEqual[Nothing, 3]]

val _ = summon[CanEqual[Byte, Nothing]]
val _ = summon[CanEqual[Nothing, Byte]]
val _ = summon[CanEqual[Short, Nothing]]
val _ = summon[CanEqual[Nothing, Short]]
val _ = summon[CanEqual[Float, Nothing]]
val _ = summon[CanEqual[Nothing, Float]]

val _ = summon[CanEqual[Double, Nothing]]
val _ = summon[CanEqual[Nothing, Double]]
val _ = summon[CanEqual[3.0, Nothing]]
val _ = summon[CanEqual[Nothing, 3.0]]

val _ = summon[CanEqual[String, Nothing]]
val _ = summon[CanEqual[Nothing, String]]
val _ = summon[CanEqual["foo", Nothing]]
val _ = summon[CanEqual[Nothing, "foo"]]

val _ = summon[CanEqual[Char, Nothing]]
val _ = summon[CanEqual[Nothing, Char]]
val _ = summon[CanEqual['f', Nothing]]
val _ = summon[CanEqual[Nothing, 'f']]

val _ = summon[CanEqual[Boolean, Nothing]]
val _ = summon[CanEqual[Nothing, Boolean]]
val _ = summon[CanEqual[true, Nothing]]
val _ = summon[CanEqual[Nothing, true]]

val _ = summon[CanEqual[Foo, Nothing]]
val _ = summon[CanEqual[Nothing, Foo]]

val _ = summon[CanEqual[Option[Int], None.type]]
val _ = summon[CanEqual[Option[Int], Option[Nothing]]]

val _ = summon[CanEqual[Any & Nothing, Foo]]
val _ = summon[CanEqual[Nothing & Any, Foo]]
