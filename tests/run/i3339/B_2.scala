package outer {
  package inner {
    object Test {
      def main(args: Array[String]): Unit = {
        println(new A())  // now ok
      }
    }
  }
}