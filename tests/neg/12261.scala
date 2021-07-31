type M0[X] = X match {
  case ? => String // error: Unbound wildcard type
}

type M1[X] = X match {
  case Any => _ // error: Unbound wildcard type
}

type M2[X] = X match {
  case Any => ? // error: Unbound wildcard type
}

val a = "" match { case _: _ => () } // error: Unbound wildcard type

val b = try { } catch { case _: _ => () } // error: Unbound wildcard type
