trait Term {
  type ap[x <: Term] <: Term
  type eval <: Term
}

// The S combinator
trait S extends Term {
  type ap[x <: Term] = S1[x]
  type eval = S
}
trait S1[x <: Term] extends Term {
  type ap[y <: Term] = S2[x, y]
  type eval = S1[x]
}
trait S2[x <: Term, y <: Term] extends Term {
  type ap[z <: Term] = S3[x, y, z]
  type eval = S2[x, y]
}
trait S3[x <: Term, y <: Term, z <: Term] extends Term {
  type ap[v <: Term] = eval#ap[v] // error: not a legal path
  type eval = x#ap[z]#ap[y#ap[z]]#eval // error: not a legal path // error: not a legal path
}

// The K combinator
trait K extends Term {
  type ap[x <: Term] = K1[x]
  type eval = K
}
trait K1[x <: Term] extends Term {
  type ap[y <: Term] = K2[x, y]
  type eval = K1[x]
}
trait K2[x <: Term, y <: Term] extends Term {
  type ap[z <: Term] = eval#ap[z] // error: not a legal path
  type eval = x#eval // error: not a legal path
}

// The I combinator
trait I extends Term {
  type ap[x <: Term] = I1[x]
  type eval = I
}
trait I1[x <: Term] extends Term {
  type ap[y <: Term] = eval#ap[y] // error: not a legal path
  type eval = x#eval // error: not a legal path
}

// Constants

trait c extends Term {
  type ap[x <: Term] = c
  type eval = c
}
trait d extends Term {
  type ap[x <: Term] = d
  type eval = d
}
trait e extends Term {
  type ap[x <: Term] = e
  type eval = e
}

case class Equals[A >: B <:B , B]()

object Test {
  type T1 = Equals[Int, Int]     // compiles fine
  type T2 = Equals[String, Int]  // error: Type argument String does not conform to upper bound Int

  type T3 = Equals[I#ap[c]#eval, c]
  type T3a = Equals[I#ap[c]#eval, d] // error: Type argument I1[c]#eval does not conform to upper bound d

  // Ic -> c
  type T4 = Equals[I#ap[c]#eval, c]

  // Kcd -> c
  type T5 = Equals[K#ap[c]#ap[d]#eval, c]

  // KKcde -> d
  type T6 = Equals[K#ap[K]#ap[c]#ap[d]#ap[e]#eval, d]

  // SIIIc -> Ic
  type T7 = Equals[S#ap[I]#ap[I]#ap[I]#ap[c]#eval, c]

  // SKKc -> Ic
  type T8 = Equals[S#ap[K]#ap[K]#ap[c]#eval, c]

  // SIIKc -> KKc
  type T9 = Equals[S#ap[I]#ap[I]#ap[K]#ap[c]#eval, K#ap[K]#ap[c]#eval]

  // SIKKc -> K(KK)c
  type T10 = Equals[S#ap[I]#ap[K]#ap[K]#ap[c]#eval, K#ap[K#ap[K]]#ap[c]#eval]

  // SIKIc -> KIc
  type T11 = Equals[S#ap[I]#ap[K]#ap[I]#ap[c]#eval, K#ap[I]#ap[c]#eval]

  // SKIc -> Ic
  type T12 = Equals[S#ap[K]#ap[I]#ap[c]#eval, c]

  // R = S(K(SI))K  (reverse)
  type R = S#ap[K#ap[S#ap[I]]]#ap[K]
  type T13 = Equals[R#ap[c]#ap[d]#eval, d#ap[c]#eval]

  type b[a <: Term] = S#ap[K#ap[a]]#ap[S#ap[I]#ap[I]]

  trait A0 extends Term {
    type ap[x <: Term] = c
    type eval = A0
  }
  trait A1 extends Term {
    type ap[x <: Term] = x#ap[A0]#eval // error: not a legal path
    type eval = A1
  }
  trait A2 extends Term {
    type ap[x <: Term] = x#ap[A1]#eval // error: not a legal path
    type eval = A2
  }

  type NN1 = b[R]#ap[b[R]]#ap[A0]
  type T13a = Equals[NN1#eval, c]

  // Double iteration
  type NN2 = b[R]#ap[b[R]]#ap[A1]
  type T14 = Equals[NN2#eval, c]

  // Triple iteration
  type NN3 = b[R]#ap[b[R]]#ap[A2]
  type T15 = Equals[NN3#eval, c]

  trait An extends Term {
    type ap[x <: Term] = x#ap[An]#eval // error: not a legal path
    type eval = An
  }

// Infinite iteration: Smashes scalac's stack
  type NNn = b[R]#ap[b[R]]#ap[An]
  // type X = Equals[NNn#eval, c]
}
