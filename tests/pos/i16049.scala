trait BatchDiffFunction[T]

abstract class FirstOrderMinimizer[T, DF <: BatchDiffFunction[T]]:
  type State = FirstOrderMinimizer.State[T]

object FirstOrderMinimizer:
  case class State[+T](x: T)

  class OptParams:
    def iterations[T](init: T): Iterator[FirstOrderMinimizer[T, BatchDiffFunction[T]]#State] = ???