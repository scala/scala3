object Outer {

  object Inner {
    class Bar(x: Int):
      def this() = this(0)
  }

  export Inner.Bar
}
