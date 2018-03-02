object Test {

  type UU[T] = ghost T => Int

  def main(args: Array[String]): Unit = {
    fun { ghost x =>
      println("lambda")
      42
    }

  }

  def fun(f: UU[Int]): Int = {
    f(35)
  }
}
