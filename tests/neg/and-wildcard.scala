object Test {

  type And[X, Y] = X & Y

  val x: And[_, _]  = ??? // error: unreducible
}