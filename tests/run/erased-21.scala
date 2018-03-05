object Test {

  type UU[T] = erased T => Int

  def main(args: Array[String]): Unit = {
    fun { erased x =>
      println("lambda")
      42
    }

  }

  def fun(f: UU[Int]): Int = {
    f(35)
  }
}
