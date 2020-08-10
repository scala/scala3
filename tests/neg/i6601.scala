object GADTs2 {
  enum Var[G, A] {
    case Z[A, G]() extends Expr[(A, G), A] // error
    case X extends AnyRef                  // error
  }
  enum Expr[G, A] {
    case Lit[G](n: Int) extends Expr[G, Int]
        // case S[A, G](x:
  }
}
