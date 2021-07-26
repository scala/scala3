import scala.collection.mutable.Builder

class DDD[S,T,A]

trait NN[S, T, A, K[_], +D <: DDD[Set[S],T,K[A]]]
class NNN[S, T, K[_], A] extends NN[S, T, A, K, DDD[Set[S],T,K[A]]]

object NN {
  def newBuilder[S, T, A, K[_]]:
      NNbuilder[S, T, K, A, DDD[Set[S],T,K[A]], NN[S,T,A,K,?], Unit] =
    new NNNbuilder[S, T, K, A]
}

// Remove the type parameter E, hardcoding in E := Unit, and the issue
// goes away.
trait NNbuilder
  [S, T, K[_], A, +D <: DDD[Set[S],T,K[A]], +N <: NN[S,T,A,K,D], E]
    extends Builder[Unit, N] {
  def clear(): Unit = throw new UnsupportedOperationException()
  final def addOne(builder: E): this.type = this
}

// Unfold this class defn, and the issue goes away
abstract class AbstractNNNbuilder
  [S, T, K[_], A, +D <: DDD[Set[S],T,K[A]], +N <: NN[S,T,A,K,D], E]
    extends NNbuilder[S,T,K,A,D,N,E]

class NNNbuilder[S, T, K[_], A] extends AbstractNNNbuilder[
  S, T, K, A, DDD[Set[S], T, K[A]], NNN[S, T, K, A], Unit
] {
  override def result(): NNN[S, T, K, A] = new NNN[S, T, K, A]
}

@main def Test: Unit = {
  val builder = NN.newBuilder[String, Char, Int, Set]
  builder += ()
  builder.result()
}