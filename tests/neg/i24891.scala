def test1: Unit = {
  val x1 = IArray("a")
  val x2 = x1.apply
  val x3: IArray[IArray[String]] = x2 // error
}
