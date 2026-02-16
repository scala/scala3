//> using options -language:experimental.modularity -source future
import compiletime.*
import compiletime.ops.int.*

/** The index of `Y` in tuple `X` as a literal constant Int,
 *  or `Size[X]` if `Y` does not occur in `X`
 */
type IndexOf[X <: Tuple, Y] <: Int = X match
  case Y *: _ => 0
  case x *: xs => S[IndexOf[xs, Y]]
  case EmptyTuple => 0

extension [X <: Tuple](inline x: X)

  /** The index (starting at 0) of the first element in the type `X` of `x`
   *  that matches type `Y`.
   */
  inline def indexOfType[Y] = constValue[IndexOf[X, Y]]

  inline def indexOf[Y: Precise](y: Y) = constValue[IndexOf[X, Y]]

// Note: without the Precise, the index calcularion would go wrong. For instance,
//  (1, 2, "hello", true).indexOf(2) would be 0, the same as  (1, 2, "hello", true).indexOTypef[Int]
//  (1, 2, "hello", true).indexOf("foo") would be 2, the same as  (1, 2, "hello", true).indexOTypef[String]
// But we could alternatively pick Singleton

@main def Test =
  val t: (1, 2, "hello", true) = (1, 2, "hello", true)
  val x1: 0 = t.indexOfType[1]
  val x2: 1 = t.indexOfType[2]
  val x3: 2 = t.indexOfType["hello"]
  val x4: 3 = t.indexOfType[true]
  val x5: 4 = t.indexOfType[77]
  val x6: 0 = t.indexOfType[Int]
  val x7: 2 = t.indexOfType[String]
  val x8: 4 = t.indexOfType[Double]

  val y1: 0 = t.indexOf(1)
  val y2: 1 = t.indexOf(2)
  val y3: 2 = t.indexOf("hello")
  val y4: 3 = t.indexOf(true)
  val y5: 4 = t.indexOf(identity(77))
  val y6: 0 = t.indexOf(identity(1))
  val y7: 4 = t.indexOf("foo")


