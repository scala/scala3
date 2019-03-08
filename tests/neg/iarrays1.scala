object Test {

  // Can't allocate an IArray

  val xs = IArray(1, 2, 3)

  // Can't have a wildcard IArray
  val ys: IArray[_] = xs   // error: unreducible application

}