object Test {

  def main(args: Array[String]): Unit = {
    println(rewrite("foo"))
    println(rewrite("foo" + "foo"))

    rewrite {
      println("apply")
    }
  }

}
