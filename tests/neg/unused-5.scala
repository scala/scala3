object Test {

  type UU[T] = unused T => Int

  def main(args: Array[String]): Unit = {
    fun { x =>
      x // error: Cannot use `unused` value in a context that is not `unused`
    }

    fun {
      (x: Int) => x // error: `Int => Int` not compatible with `unused Int => Int`
    }
  }

  def fun(f: UU[Int]): Int = {
    f(35)
  }
}
