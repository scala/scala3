trait Lam:
  type F[_]
  extension [A, B](f: F[A => B]) def apply(arg: F[A]): F[B]
  def lam[A, B](f: F[A] => F[B]): F[A => B]
  final def id[A]: F[A => A] = lam(identity[F[A]])

object LamInterpreter extends Lam:
  type F[t] = t
  def lam[A, B](f: F[A] => F[B]): F[A => B] = f
  extension [A, B](f: F[A => B]) def apply(arg: F[A]): F[B] = f(arg)


class Church[A](using val l: Lam):
  import l.*
  type Nat = F[(A => A) => (A => A)]
  def zero: Nat = id
  extension (n: Nat) def suc: Nat = lam(f => lam(x => f(n(f)(x))))

given [A](using l: Lam): Church[A] = Church()


@main
def churchTest =
  given Lam = LamInterpreter
  val c: Church[Int] = summon
  summon[c.Nat =:= ((Int => Int) => (Int => Int))] // error (not a compiler crash)
