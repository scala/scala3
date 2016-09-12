object Test {
  def main(args: Array[String]): Unit = {
    (new Function0[Unit] {
      def apply() = println("working")
    })()
  }
}
