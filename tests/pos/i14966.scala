trait I[+A] extends IOps[A, I[A]]

trait S[A] extends I[A], SOps[A, S[A]]

trait IOps[+A, +C <: I[A]]:
  def concat[B >: A](other: IterableOnce[B]): C

trait SOps[A, +C <: S[A]] extends IOps[A, C]:
  def concat(other: IterableOnce[A]): C

class Test(s: S[Int]):
  export s.*

