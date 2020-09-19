object Test {

  def main(args: Array[String]): Unit = {
    val someFoo = Some("foo")
    val _ as bar = someFoo
    val _ as Some(baz) = someFoo
    println(bar)
    println(baz)
  }

}
