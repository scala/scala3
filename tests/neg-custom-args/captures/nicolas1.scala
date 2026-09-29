import caps.*

trait Rand extends SharedCapability:
  def range(min: Int, max: Int): Int

def nextInt(max: Int): Rand ?-> Int =
  r ?=> r.range(0, max)

def oneOf[A, c^](head: Rand ?->{c} A, tail: (Rand ?->{c} A)*): Rand ?->{head} A =
  val all: Seq[Rand ?->{c} A] = head +: tail
  all(nextInt(all.length)) // error
