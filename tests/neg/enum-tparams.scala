object Test {

  enum Opt[+T] {
    case S(x: T) extends Opt[T]
    case I(x: Int) extends Opt[Int]
    case V() extends Opt[`T`]
    case P(x: List[T]) extends Opt[String]
    case N extends Opt[Nothing]
  }

  type Id[_]

  enum E[F[_], G[_]] {
    case C1() extends E[[X] =>> X, Id]
    case C2() extends E[[F] =>> F, Id]
    case C3() extends E[[X] =>> { type Y = F[Int] }, Id]
    case C4() extends E[[X] =>> { type F = Int }, Id]
    case C5() extends E[[F] =>> G[Int], Id]
  }

  Opt.S[Int](1) // OK
  Opt.S(1)      // OK
  Opt.I[Int](1) // error: does not take type parameters
  Opt.I(1)      // OK
  Opt.V[Int]()  // OK
  Opt.V()       // OK
  Opt.P[Int](List(1, 2, 3)) // OK
  Opt.P(List(1, 2, 3)) // OK

  E.C1[List, Id]()  // error: does not take type parameters
  E.C1()            // OK
  E.C2[List, Id]()  // error: does not take type parameters
  E.C2()            // OK
  E.C3[List, Id]()  // OK
  E.C3()            // OK
  E.C4[List, Id]()  // error: does not take type parameters
  E.C4()            // OK
  E.C5[List, Id]()  // OK
  E.C5()            // OK
}