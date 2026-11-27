trait In[F[_], G[_]]
object In:
  given self[F[_]]: In[F, F] = new In {}
  given deeper[F[_], G[_], H[_]](using In[F, G]): In[F, [A] =>> G[A] | H[A]] = new In {}

trait IO[A]
trait Log[A]

def needs[F[_], G[_]](using In[F, G]): Int = 0

val here = needs[IO, IO]
val deep = needs[IO, [A] =>> IO[A] | Log[A]]

// Joining two different uninstantiated type variables used to crash with
// "Failure to join alternatives F and F" after `F =:= G` unified them.
def f[F[_], G[_], T]: F[T] | G[T] = ???
def x = f.toString
