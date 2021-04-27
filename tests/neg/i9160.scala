package foo

object Test {
  type F[T]
  val F: F[Int] = ???

  def f[T](t: T)(
    e1: F[T], e2: F[T], e3: F[T],
    e4: F[T], e5: F[T], e6: F[T],
    e7: F[T], e8: F[T], e9: F[T],
    e10: F[T], e11: F[T], e12: F[T],
  ) = ???
  f("foo")(
    F, F, F, // error // error // error
    F, F, F, // error // error // error
    F, F, F, // error // error // error
    F, F, F, // error // error // error
  )
}


object x extends X1 with X2

trait X1 extends Implicits
trait X2 extends Implicits

trait Implicits {
  type F[T]
  type G[T]

  given g1 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g2 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g3 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g4 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g5 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g6 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g7 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g8 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g9 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g10 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g11 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g12 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g13 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g14 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g15 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g16 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g17 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g18 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g19 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g20 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g21 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g22 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g23 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g24 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g25 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g26 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g27 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g28 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g29 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g30 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g31 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g32 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g33 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g34 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g35 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g36 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g37 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g38 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g39 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g40 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g41 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g42 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g43 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g44 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g45 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g46 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g47 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g48 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g49 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g50 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g51 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g52 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g53 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g54 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g55 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g56 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g57 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g58 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g59 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g60 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g61 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g62 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g63 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g64 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g65 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g66 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g67 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g68 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g69 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g70 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g71 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g72 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g73 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g74 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g75 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g76 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g77 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g78 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g79 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g80 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g81 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g82 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g83 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g84 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g85 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g86 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g87 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g88 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g89 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g90 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g91 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g92 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g93 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g94 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g95 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g96 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g97 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g98 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g99 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
  given g100 [T1: F, T2: F, T3: F, T4: F, T5: F, T6: F, T7: F, T8: F, T9: F, T10: F, T11: F, T12: F, T13: F, T14: F, T15: F, T16: F, T17: F, T18: F, T19: F, T20: F, T21: F, T22: F]: G[Int] = ???
}