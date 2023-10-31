object Test {

  type And[X, Y] = X & Y

  val x: And[?, ?]  = ??? // error: unreducible
}