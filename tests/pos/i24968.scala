enum Type[A] {
  case TString extends Type[String]
  case TInt extends Type[Int]
}

enum DSL {
  case Run[A](value: A, type0: Type[A])
  case Noop
}

object Eval {
  def loop(dsl: DSL) =
    dsl match {
      case DSL.Run(value, Type.TString) => 42
      case DSL.Run(value, Type.TInt)    => 42
      case DSL.Noop                     => 42
    }
}
