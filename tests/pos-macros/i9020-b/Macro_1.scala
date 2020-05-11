trait Show[T] {
  def show(t: T): String
}

object Show {
  inline def deriveWithMacro[T]: Show[T] = ${ impl[T] }

  import quoted._
  def impl[T](using s: Scope)(using tpe: s.Type[T]): s.Expr[Show[T]] =
    '{
      new Show[$tpe] {
        def show(t: $tpe): String = "TODO"
      }
    }
}
