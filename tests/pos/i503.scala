class HelloWorld {
  def main(args: Array[String]): Unit = {
    object TypeBool;

    class Fct {
      def g(x : Int) = TypeBool // breaks.
    }
  }
}
