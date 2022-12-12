final abstract class ForcedRecompilationToken[T]
object ForcedRecompilationToken {
  implicit def default: ForcedRecompilationToken["abc"] = null
}

class BadNoParens[T](implicit ev: ForcedRecompilationToken[T])

// error
object X extends BadNoParens

// ok
object Y extends BadNoParens()

object App extends App {
  println("compiled")
}