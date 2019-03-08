object Test {

  trait Effect

  // Type X => Y
  abstract class Fun[-X, Y] {
    type Eff <: Effect
    def apply(x: X): given Eff => Y
  }

  // Type X -> Y
  type PureFun[-X, Y] = Fun[X, Y] { type Eff = Effect }

  // def map(f: A => B)(xs: List[A]): List[B]
  def map[A, B, E <: Effect](f: Fun[A, B] { type Eff = E})(xs: List[A])
    : given E => List[B] =
    xs.map(f.apply)

  // def mapFn[A, B]: (A => B) -> List[A] -> List[B]
  def mapFn[A, B, E <: Effect]:
    PureFun[
      Fun[A, B] { type Eff = E},
      Fun[List[A], List[B]] { type Eff = E }
    ] =
    new Fun[ // would like to write new PureFun here
             // or, even better, drop everything
      Fun[A, B] { type Eff = E},
      Fun[List[A], List[B]] { type Eff = E }
    ] {
      type Eff = Effect
      def apply(f: Fun[A, B] { type Eff = E}) =
        new Fun[List[A], List[B]] {
          type Eff = E
          def apply(xs: List[A]): given Eff => List[B] =
            map(f)(xs)
        }
    }

  implicit def combine[E1 <: Effect, E2 <: Effect] given (x: E1, y: E2): E1 & E2 = ???

  // def compose(f: A => B)(g: B => C)(x: A): C
  def compose[A, B, C, E1 <: Effect, E2 <: Effect]
    (f: Fun[A, B] { type Eff = E1})
    (g: Fun[B, C] { type Eff = E2})
    (x: A):
    given E1 & E2 => C = g(f(x))

  // def composeFn: (A => B) -> (B => C) -> A -> C
  def composeFn[A, B, C, E1 <: Effect, E2 <: Effect]:
    PureFun[
      Fun[A, B] { type Eff = E1},
      PureFun[
        Fun[B, C] { type Eff = E2},
        Fun[A, C] { type Eff = E1 & E2 }
      ]
    ] =
    new Fun[
      Fun[A, B] { type Eff = E1},
      PureFun[
        Fun[B, C] { type Eff = E2},
        Fun[A, C] { type Eff = E1 & E2 }
      ]
    ] {
      type Eff = Effect
      def apply(f: Fun[A, B] { type Eff = E1}) =
        new Fun[
          Fun[B, C] { type Eff = E2},
          Fun[A, C] { type Eff = E1 & E2 }
        ] {
          type Eff = Effect
          def apply(g: Fun[B, C] { type Eff = E2}) =
            new Fun[A, C] {
              type Eff = E1 & E2
              def apply(x: A) = compose(f)(g)(x)
            }
        }
    }
}