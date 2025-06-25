def f(x: Int): Int = ???
def id[T](x: T): T = x
def opaqueSize[T](l: List[T]): Int = ???

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
