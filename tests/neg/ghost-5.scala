object Test {

  type UU[T] = ghost T => Int

  def main(args: Array[String]): Unit = {
    fun { x =>
      x // error: Cannot use `ghost` value in a context that is not `ghost`
    }

    fun {
      (x: Int) => x // error: `Int => Int` not compatible with `ghost Int => Int`
    }
  }

  def fun(f: UU[Int]): Int = {
    f(35)
  }
}
