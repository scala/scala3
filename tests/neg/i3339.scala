package outer {
  private class A

  package inner {
    object Test {
      def main(args: Array[String]): Unit = {
        println(new A)  // error: cannot access
      }
    }
  }
}
