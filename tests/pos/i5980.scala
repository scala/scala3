trait A
trait B

trait Covariant[F[+_]] {
  trait G[+X]

  def fx: F[A  &   B] = ???
  def fy: F[A] & F[B] = fx

  def gx: G[A  &   B] = ???
  def gy: G[A] & G[B] = gx
}

trait Contravariant[F[-_]] {
  trait G[-X]

  def fx: F[A  |   B] = ???
  def fy: F[A] & F[B] = fx

  def gx: G[A  |   B] = ???
  def gy: G[A] & G[B] = gx
}

trait LiskovViolation[F[+_]] {
  trait A { def children: F[A] }
  trait B { def children: F[B] }
  trait C extends A with B { def children: F[A] & F[B] = ??? }

  def fc1: C     = new C {}
  def fc2: A & B = fc1
  def fy2: F[A] & F[B] = fc2.children
}
