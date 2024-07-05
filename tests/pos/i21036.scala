//> using options -source 3.5 -Werror
trait SameRuntime[A, B]
trait BSONWriter[T]
trait BSONHandler[T] extends BSONWriter[T]

opaque type Id = String
object Id:
  given SameRuntime[Id, String] = ???

given BSONHandler[String]                    = ???
given [T: BSONHandler]: BSONHandler[List[T]] = ???

given opaqueWriter[T, A](using rs: SameRuntime[T, A], writer: BSONWriter[A]): BSONWriter[T] = ???

val x = summon[BSONHandler[List[Id]]] // this doesn't emit warning
val y = summon[BSONWriter[List[Id]]] // this did emit warning
