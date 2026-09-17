// Implicit search over a row encoded as a union of abstract type
// constructors used to crash orDominator with
// "AssertionError: Failure to join alternatives G and H".
// It must report a missing given instead.
trait In[F[_], G[_]]
object In:
  given deeper[F[_], G[_], H[_]](using In[F, G]): In[F, [A] =>> G[A] | H[A]] = new In {}

def needs[F[_], G[_]](using In[F, G]): Int = 0

def boom[F[_], G[_]]: Int = needs[F, G] // error
