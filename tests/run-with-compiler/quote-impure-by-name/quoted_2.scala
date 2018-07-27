object Test {

  def main(args: Array[String]): Unit = {
    println(Index.succ["bar", "foo", ("bar", ("baz", Unit))])
  }
}
