import caps.*

trait Rand extends SharedCapability:
  def range(min: Int, max: Int): Int

def nextInt(max: Int): Rand ?-> Int =
  r ?=> r.range(0, max)

def oneOf[A, B^](head: Rand ?=> A, tail: (Rand ?->{B} A)*): Rand ?->{head, B} A =
  val all: Seq[Rand ?->{head, B} A] = head +: tail // error
  all(nextInt(all.length))