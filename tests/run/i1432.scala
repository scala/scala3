object Test {

  def main(args: Array[String]): Unit = {
    val someFoo = Some("foo")
    val bar = someFoo
    val Some(baz) = someFoo
    println(bar)
    println(baz)
  }

}
