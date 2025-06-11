def id[T](x: T): T = x

abstract class C:
  type T
  val x: T

def test: Unit =
  val x: Int = ???
  val x2: Int = x
  val x3: Int = x2
  val y: Int = x + 1
  val c1: C = ???
  val c2: C = ???

  summon[{v: Int with v == x} <:< {v: Int with v == x2}]
  summon[{v: Int with v == x2} <:< {v: Int with v == x}]
  summon[{v: Int with v == x} <:< {v: Int with v == x3}]
  summon[{v: Int with v == x3} <:< {v: Int with v == x}]
  summon[{v: Int with v == x2} <:< {v: Int with v == x3}]
  summon[{v: Int with v == x3} <:< {v: Int with v == x2}]

  summon[{v: Int with v == y} <:< {v: Int with v == x + 1}]
  summon[{v: Int with v == x + 1} <:< {v: Int with v == y}]

  summon[{v: Int with v == id(x)} <:< {v: Int with v == id(x2)}]
  summon[{v: Int with v == id(x2)} <:< {v: Int with v == id(x)}]
  //summon[{v: Int with v == id(y)} <:< {v: Int with v == id(x + 1)}] // TODO(mbovel): needs normaliazion of type hashes
  //summon[{v: Int with v == id(x + 1)} <:< {v: Int with v == id(y)}] // TODO(mbovel): needs normaliazion of type hashes

  summon[{v: Int with v == y + 2} <:< {v: Int with v == x + 1 + 2}]
  summon[{v: Int with v == x + 1 + 2} <:< {v: Int with v == y + 2}]

  summon[{v: Int with v == id[c1.T](c1.x)} <:< {v: Int with v == id[c1.T](c1.x)}]

  def innerScope() =
    summon[{v: Int with v == x} <:< {v: Int with v == x2}]
    summon[{v: Int with v == x2} <:< {v: Int with v == x}]
    summon[{v: Int with v == x} <:< {v: Int with v == x3}]
    summon[{v: Int with v == x3} <:< {v: Int with v == x}]
    summon[{v: Int with v == x2} <:< {v: Int with v == x3}]
    summon[{v: Int with v == x3} <:< {v: Int with v == x2}]

    summon[{v: Int with v == y} <:< {v: Int with v == x + 1}]
    summon[{v: Int with v == x + 1} <:< {v: Int with v == y}]

    summon[{v: Int with v == id(x)} <:< {v: Int with v == id(x2)}]
    summon[{v: Int with v == id(x2)} <:< {v: Int with v == id(x)}]
    //summon[{v: Int with v == id(y)} <:< {v: Int with v == id(x + 1)}] // TODO(mbovel): needs normaliazion of type hashes
    //summon[{v: Int with v == id(x + 1)} <:< {v: Int with v == id(y)}] // TODO(mbovel): needs normaliazion of type hashes

    summon[{v: Int with v == y + 2} <:< {v: Int with v == x + 1 + 2}]
    summon[{v: Int with v == x + 1 + 2} <:< {v: Int with v == y + 2}]

    summon[{v: Int with v == id[c1.T](c1.x)} <:< {v: Int with v == id[c1.T](c1.x)}]
