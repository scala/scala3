  type Nat = [A] => (A => A) => A => A

  def Zero: Nat =
    [A] => (f: A => A) => (a: A) => a

  def Succ[A](n: Nat): Nat =
    [A] => (f: A => A) => (a: A) => f(n(f)(a))

  def one: Nat =
    Succ(Zero)

  def mul(m: Nat)(n: Nat): Nat =
    [A] => (f: A => A) => (a: A) => m(n(f))(a)

  def exp(m: Nat)(n: Nat): Nat =
    // the compiler throws a StackOverflowError without the type annotations!
    [A] => (f: A => A) => (a: A) => (n[Nat](mul(m))(one): Nat)(f)(a)

