
object Test {

  def g[T] = ()

  g[Pure]     // error
  g[Impure]   // error

  def h[E >: Impure <: Pure] = ()

  h[Any]      // error
  h[Nothing]  // error
}
