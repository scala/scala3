// Implicit search over a union of abstract type constructors
// used to crash the compiler in `TypeOps.orDominator` with
// "AssertionError: Failure to join alternatives G and H".
// It must report a missing given instead.
object differentParams:
  trait In[F[_], G[_]]
  object In:
    given deeper[F[_], G[_], H[_]](using In[F, G]): In[F, [A] =>> G[A] | H[A]] = new In {}

  def needs[F[_], G[_]](using In[F, G]): Int = 0

  def boom[F[_], G[_]]: Int = needs[F, G] // error

// Same crash with "Failure to join alternatives G and Seq": the type
// parameter G and the class Seq have the same type symbol.
object paramAndClass:
  trait In[F[_], G[_]]
  object In:
    given deeper[F[_], G[X] <: Seq[X]](using In[F, G]): In[F, [A] =>> G[A] | Seq[A]] = new In {}

  def needs[F[_], G[_]](using In[F, G]): Int = 0

  def boom[F[_], G[_]]: Int = needs[F, G] // error
