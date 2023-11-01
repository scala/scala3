object Test {

  def test() = {
    java.util.Arrays.asList(Array(1,2,3)*)
  }

  def main(args: Array[String]): Unit = {
    println(test())
  }

}
