class Test {
  import Test.test
  "Hello".toto     // error
}

object Test {
  def test = {
    implicitly[collection.generic.CanBuildFrom[List[Int], Int, List[Int]]]
  }
}
