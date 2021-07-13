case class Var(name: String)

@main def f = Var("a") match
  case Var(name, _) => name: String // error: Wrong number of argument patterns for Var; expected: (String)
