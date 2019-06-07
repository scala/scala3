object Test {

  def main(args: Array[String]): Unit = {
    Index.succ["bar", "foo", ("bar", ("baz", Unit))]
  }
}
