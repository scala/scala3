enum Term[T <: Term[T]] {
    case Wrap(t: T)
    case Fun(id: Id, tag: K, ret: Term[T])  // error // error
  }

  enum Type {
    case Var(id: Id)  // error
  }

  val tExp: Term[Type, Kind] =  // error
    Term.Fun("x", Term.Wrap(Type.Var("x"))) // error