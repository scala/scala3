type Id = String

enum Kind {
    case Type
  }

  enum Term[T <: Term[T, K], K] {
    case Wrap(t: T)
    case Fun(id: Id, tag: K, ret: Term[T, K])
  }

  enum Type {
    case Var(id: Id)
  }

  val tExp: Term[Type, Kind] =
    Term.Fun("x", Kind.Type, Term.Wrap(Type.Var("x")))  // error

  def main(args: Array[String]): Unit = { }