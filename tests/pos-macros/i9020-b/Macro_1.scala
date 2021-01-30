trait Show[T] {
  def show(t: T): String
}

object Show {
  inline def deriveWithMacro[T]: Show[T] = ${ impl[T] }

  import quoted.*
  def impl[T](using ctx: Quotes, tpe: Type[T]): Expr[Show[T]] =
    '{
      new Show[tpe.Underlying] {
        def show(t: tpe.Underlying): String = "TODO"
      }
    }
}
