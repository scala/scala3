object Test {

  def main(args: Array[String]): Unit = {
    val someFoo = Some("foo")
    val _ @ bar = someFoo
    val _ @ Some(baz) = someFoo
    println(bar)
    println(baz)
  }

}
