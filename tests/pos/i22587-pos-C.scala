// Positive Test Case C: Accumulator Pattern with Iterator Bounds
// This should NOT diverge because Idx increments toward Limit, guaranteeing termination.
// Replicates patterns like regex accumulators using iterator and end-of-iteration arguments.

import scala.compiletime.ops.int.*

type Loop[Idx <: Int, Limit <: Int, Acc <: Tuple] <: Tuple = Idx match
  case Limit => Acc
  case _     => Loop[Idx + 1, Limit, "Token" *: Acc]

type Result = Loop[0, 5, EmptyTuple]