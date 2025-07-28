//> using options -experimental

import scala.util.TupledFunction
import scala.util.NotGiven

object Test {
  type T

  summon[TupledFunction[(x: T, y: T) => x.type, (x: (T, T)) => x._1.type]]

  // arity > 22
  summon[TupledFunction[
    (a: T, b: T, c: T, d: T, e: T, f: T, g: T, h: T, i: T, j: T, k: T, l: T, m: T, n: T, o: T, p: T,
    q: T, r: T, s: T, t: T, u: T, v: T, w: T) => a.type,
    (a: (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T)) => Tuple.Elem[a.type, 0]
    ]]
}