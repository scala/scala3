object Test {

  def main(args: Array[String]): Unit = {
    rewrite(Nil.map(x => x))
    rewrite(Nil.map(x => x) ++ Nil.map(x => x))
    rewrite(Nil.map(x => x) ++ List(3) ++ Nil)
  }

}
