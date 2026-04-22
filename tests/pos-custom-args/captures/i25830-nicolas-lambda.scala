import language.experimental.captureChecking
import caps.*

trait Rand extends SharedCapability:
  def range(min: Int, max: Int): Int

val oneOf: [A, B^] => (head: Rand ?=> A, tail: Seq[Rand ?->{B} A]) => Seq[Rand ?->{head, B} A] =
  { [A, B^] => (head: Rand ?=> A, tail: Seq[Rand ?->{B} A]) =>
    val all: Seq[Rand ?->{head, B} A] = head +: tail
    all
  }
