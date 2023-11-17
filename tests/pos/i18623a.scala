final abstract class ForcedRecompilationToken[T]
object ForcedRecompilationToken {
  implicit def default: ForcedRecompilationToken["abc"] = null
}

object x {
class GoodNoParens[T](implicit ev: ForcedRecompilationToken[T])
}
export x.GoodNoParens as BadNoParens

// error
object A extends BadNoParens

// ok
object B extends BadNoParens()
object C extends x.GoodNoParens

object App extends App {
  println("compiled")
}