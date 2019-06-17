object Test {
  enum Expr[+T] {
    case BoolLit(b: Boolean) extends Expr[Boolean]
    def eval: T = {
      def go[TT](self: this.type & Expr[TT]): TT = self match {
        case BoolLit(b) => b
      }

      go(this)
    }
  }
}
