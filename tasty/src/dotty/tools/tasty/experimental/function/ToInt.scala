package dotty.tools.tasty.experimental.function

  @FunctionalInterface
  trait ToInt[A] with
    def apply(a: A): Int
