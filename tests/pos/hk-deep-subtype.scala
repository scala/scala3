// Minimized from scala.collection.generic.GenTraversableFactory plus dependencies
import scala.annotation.unchecked.uncheckedVariance

trait GT[A] extends GTT[A, GT]

trait HNB[B]
trait GTT[+C, DD[X] <: GT[X]] extends HNB[DD[C] @uncheckedVariance] // Can be any annotation and still crash

class GTF[EE[X] <: GT[X] with GTT[X, EE]]
{
  def foo[F]: EE[F] = ???
  def bar[G](f: G): EE[G] = ???

  def tabulate: EE[EE[Int]] = bar(foo)
}
