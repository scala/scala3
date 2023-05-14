package p

object M {
  class C[N]()
}

export M.*

type CC[N] = M.C[N]
type CCC = M.C[Int]
