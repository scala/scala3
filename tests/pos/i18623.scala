final abstract class ForcedRecompilationToken[T]
object ForcedRecompilationToken {
  implicit def default: ForcedRecompilationToken["abc"] = null
}

class GoodNoParens[T](implicit ev: ForcedRecompilationToken[T])
type BadNoParens[T] = GoodNoParens[T]

// error
object A extends BadNoParens

// ok
object B extends BadNoParens()
object C extends GoodNoParens

