trait Show[T] {
  def show(t: T): String
}

object Show {
  inline def deriveWithMacro[T]: Show[T] = ${ impl[T] }

  import quoted._
  def impl[T](using ctx: QuoteContext, tpe: Staged[T]): Expr[Show[T]] =
    '{
      new Show[$tpe] {
        def show(t: $tpe): String = "TODO"
      }
    }
}
