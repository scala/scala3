//> using options -Wunused:patvars -Werror

def make: IndexedSeq[FalsePositive] =
  for {
    i <- 1 to 2
    given Int = i
    fp = FalsePositive()
  } yield fp

def broken =
  for
    i <- List(42)
    (x, y) = "hello" -> "world"
  yield
    s"$x, $y" * i

def alt: IndexedSeq[FalsePositive] =
  given String = "hi"
  for
    given Int <- 1 to 2
    j: Int = summon[Int] // simple assign because irrefutable
    _ = j + 1
    k :: Nil = j :: Nil : @unchecked // pattern in one var
    fp = FalsePositive(using k)
  yield fp

class FalsePositive(using Int):
  def usage(): Unit =
    println(summon[Int])
