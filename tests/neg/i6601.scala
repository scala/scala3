object GADTs2 {
  enum Var[G, A] {
    case Z[A, G]() extends Expr[(A, G), A] // error
    case X extends AnyRef                  // error // error
  }
  enum Expr[G, A] {
    case Lit[G](n: Int) extends Expr[G, Int]
        // case S[A, G](x:
  }
  enum Covariant[+T] {
    case Bottom extends AnyRef // error // error
  }
  enum Contravariant[-T] {
    case Top extends AnyRef // error // error
  }
  enum Color {
    case Red extends AnyRef // error // error
  }
  enum Parameterized[T](x: T) {
    case Foo extends AnyRef // error // error
  }
}
