import reflect.Selectable.reflectiveSelectable

class Test {
  trait A
  val m: A { def apply(x: Int): Int } =
    new A {
      def apply(x: Int): Int =
        n + x
    }

  val n = m(23)   // warn
}
