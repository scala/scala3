class Test {
  import Test.test
  "Hello".toto
}

object Test {
  def test = {
    implicitly[collection.generic.CanBuildFrom[List[Int], Int, List[Int]]]
  }
}
