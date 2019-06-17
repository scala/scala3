trait C { this: { type T = Int } =>

  type T
  val x: T
}

class D extends C {
  type T = Int
  val x = 10
}

object Test {

  val c: C = new D
  val y: c.T = c.x
  val x: Int = c.x   // error
  val z: Int = y     // error
}