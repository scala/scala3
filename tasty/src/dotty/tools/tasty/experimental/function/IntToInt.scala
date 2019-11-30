package dotty.tools.tasty.experimental.function

  @FunctionalInterface
  trait IntToInt with
    def apply(int: Int): Int
