def Test = {
  val ops: CompileOps[Option, Option, Any]  = ???
  ops.to(List).map(_.reverse) // error
}

trait CompileOps[F[_], G[_], O]:
  def to(collector: Collector[O]): G[collector.Out]
trait Collector[-A] {
  type Out
}

object Collector:
  type Aux[A, X] = Collector[A] { type Out = X }

  given [A, C[_]]: Conversion[
    scala.collection.IterableFactory[C],
    Collector.Aux[A, C[A]]
  ] = ???
