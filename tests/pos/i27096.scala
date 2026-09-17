// The same encoding with a CONCRETE row still resolves.
trait In[F[_], G[_]]
object In:
  given self[F[_]]: In[F, F] = new In {}
  given deeper[F[_], G[_], H[_]](using In[F, G]): In[F, [A] =>> G[A] | H[A]] = new In {}

trait IO[A]
trait Log[A]

def needs[F[_], G[_]](using In[F, G]): Int = 0

val here = needs[IO, IO]
val deep = needs[IO, [A] =>> IO[A] | Log[A]]
