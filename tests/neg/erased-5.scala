object Test {

  type UU[T] = erased T => Int

  def main(args: Array[String]): Unit = {
    fun { x =>
      x // error: Cannot use `erased` value in a context that is not `erased`
    }

    fun {
      (x: Int) => x // error: `Int => Int` not compatible with `erased Int => Int`
    }
  }

  def fun(f: UU[Int]): Int = {
    f(35)
  }
}
