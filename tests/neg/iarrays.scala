object Test {

  // Can't allocate an IArray
  new IArray[String](10)  // error: not a class type // error: too many arguments

  val xs = IArray(1, 2, 3)

  // Can't have a wildcard IArray
  val ys: IArray[_] = xs

  // Can't update an IArray
  xs(0) = 1    // error: value update is not a member
  xs(1) += 1   // error: value += is not a member

}