final abstract class ForcedRecompilationToken[T]
object ForcedRecompilationToken {
  implicit def default: ForcedRecompilationToken["abc"] = null
}

object x {
  abstract class GoodNoParens[T](implicit ev: ForcedRecompilationToken[T])
}
type BadNoParens[T] = x.GoodNoParens[T]

object App extends App {
  new BadNoParens {}
  new BadNoParens() {}
  new x.GoodNoParens {}
}
