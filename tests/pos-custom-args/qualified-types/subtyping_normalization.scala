def f(x: Int): Int = ???
def id[T](x: T): T = x
def opaqueSize[T](l: List[T]): Int = ???
class Box(val size: Int)

def test: Unit =
  val x: Int = ???
  val y: Int = ???
  val z: Int = ???

  summon[{v: Int with v == 2 + (x * y * y * z)} <:< {v: Int with v == (x * y * z * y) + 2}]
  summon[{v: Int with v == x + 1}            <:< {v: Int with v == 1 + x}]
  summon[{v: Int with v == y + x}            <:< {v: Int with v == x + y}]
  summon[{v: Int with v == x + 2}            <:< {v: Int with v == 1 + x + 1}]
  summon[{v: Int with v == x + 2}            <:< {v: Int with v == 1 + (x + 1)}]
  summon[{v: Int with v == x + 2 * y}        <:< {v: Int with v == y + x + y}]
  summon[{v: Int with v == x + 2 * y}        <:< {v: Int with v == y + (x + y)}]
  summon[{v: Int with v == x + 3 * y}        <:< {v: Int with v == 2 * y + x + y}]
  summon[{v: Int with v == x + 3 * y}        <:< {v: Int with v == 2 * y + (x + y)}]
  summon[{v: Int with v == 0}                <:< {v: Int with v == 1 - 1}]
  summon[{v: Int with v == 0}             <:< {v: Int with v == x - x}]
  summon[{v: Int with v == 0}                <:< {v: Int with v == x + (x * -1)}]
  summon[{v: Int with v == x}             <:< {v: Int with v == 1 + x - 1}]
  summon[{v: Int with v == 4 * (x + 1)}      <:< {v: Int with v == 2 * (x + 1) + 2 * (1 + x)}]
  summon[{v: Int with v == 4 * (x / 2)}      <:< {v: Int with v == 2 * (x / 2) + 2 * (x / 2)}]

  summon[{v: Int with v == id(x + 1)}        <:< {v: Int with v == id(1 + x)}]
  summon[{v: Int with v == id(x + 1)}        <:< {v: Int with v == id(x + 1)}]

  summon[{v: List[Int] with opaqueSize(v) == 2 * x} <:< {v: List[Int] with opaqueSize(v) == x + x}]

  // Comparison normalization: >= to <=, > to <, <= to <
  summon[{v: Int with x >= y} <:< {v: Int with y <= x}]
  summon[{v: Int with x >= y} <:< {v: Int with y < x + 1}]
  summon[{v: Int with x > y}  <:< {v: Int with y < x}]
  summon[{v: Int with x <= y} <:< {v: Int with x < y + 1}]
  summon[{v: Int with 0 <= v} <:< {v: Int with v >= 0}]
  summon[{v: Int with v >= 0} <:< {v: Int with 0 <= v}]

  // Transitivity of <
  summon[{v: Int with x < y && y < z} <:< {v: Int with x < z}]
  // Chained transitivity: a < b, b < c, c < d => a < d
  summon[{v: Int with x < y && y < z && z < v} <:< {v: Int with x < v}]
  // Transitivity across separate pairs: a < b, c < d, b < c => a < d
  summon[{v: Int with x < y && z < v && y < z} <:< {v: Int with x < v}]
  // Transitivity with constants
  summon[{v: Int with v < 5 && 5 < x} <:< {v: Int with v < x}]
  // Transitivity with function calls
  summon[{v: Int with v < f(x) && f(x) < y} <:< {v: Int with v < y}]
  summon[{v: Int with x < id(y) && id(y) < z} <:< {v: Int with x < z}]
  // Transitivity with paths
  val b: Box = ???
  summon[{v: Int with v < b.size && b.size < x} <:< {v: Int with v < x}]
  // Reflexive/cyclic cases don't loop
  summon[{v: Int with x < x} <:< {v: Int with x < x}]
  summon[{v: Int with x < y && y < x} <:< {v: Int with x < x}]
