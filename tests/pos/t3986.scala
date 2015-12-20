object Test {
  def main(args: Array[String]): Unit = {
    new AnyRef {
// TODO NEEDS MANUAL CHANGE (early initializers)
// BEGIN copied early initializers
val x = "abc"
// END copied early initializers
 }
  }
}
