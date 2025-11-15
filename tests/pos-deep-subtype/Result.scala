//> using options -Werror

object p {

  // test parametric case classes, which synthesis `canEqual` and `equals`
  enum Result[+T, +E] {
    case OK [T](x: T) extends Result[T, Nothing]
    case Err[E](e: E) extends Result[Nothing, E]
  }
}
