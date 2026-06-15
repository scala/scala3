import compiletime.constValueOpt
transparent inline def foo[L, R]: Boolean =
  inline (constValueOpt[L], constValueOpt[R]) match
    case (Some(l), Some(r)) => true
    case _                  => false
transparent inline def fooWorks[L, R]: Boolean =
  inline constValueOpt[L] match
    case Some(l) =>
      inline constValueOpt[R] match
        case Some(l) => true
        case _       => false
    case _ => false

val f: true = foo[1, 2] //error here
val fWorks: true = fooWorks[1, 2]
