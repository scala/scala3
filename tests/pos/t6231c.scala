object Bug {
  def bar(ev: Any) = {
    trait X(val x: Int) {
      def qux: () => x.type = { () => println(ev); x }
    }
    (new X(1) {}).qux()
  }
}

