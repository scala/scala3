// Test for issue #24891: IArray.apply overload resolution bug
// IArray(x1) should invoke the factory method, not the extension method

object Test {
  val x1 = IArray("a")           // Creates IArray[String]
  val x2 = IArray(x1)            // Should create IArray[IArray[String]]
  val x3: IArray[IArray[String]] = x2  // This should compile
}
