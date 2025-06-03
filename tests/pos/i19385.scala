import scala.compiletime.summonAll

inline def f[M <: Tuple]: Unit =
  type Alias = Tuple.Map[M, [X] =>> Numeric[X]]
  summonAll[Tuple.Map[M, [X] =>> Numeric[X]]] // compiles
  summonAll[Alias] // error: Tuple element types must be known at compile time

val y1 = f[(Int, Int)]