object Test {

  type UU[T] = unused T => Int

  def main(args: Array[String]): Unit = {
    fun { unused x =>
      println("lambda")
      42
    }

  }

  def fun(f: UU[Int]): Int = {
    f(35)
  }
}
