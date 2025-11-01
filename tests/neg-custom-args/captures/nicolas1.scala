import caps.*

trait Rand extends SharedCapability:
  def range(min: Int, max: Int): Int

def nextInt(max: Int): Rand ?-> Int =
  r ?=> r.range(0, max)

def oneOf[A](head: Rand ?=> A, tail: (Rand ?=> A)*): Rand ?->{head} A =
  val all: Seq[Rand ?->{head, tail*} A] = head +: tail // error
  all(nextInt(all.length))