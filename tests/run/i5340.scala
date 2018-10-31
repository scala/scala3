object Test {
  def main(args: Array[String]): Unit = {
    lazy val a: Int = { println("Int"); 1 }
    lazy val b: Long = { println("Long"); 1L }
    lazy val c: Float = { println("Float"); 1F }
    lazy val d: Double = { println("Double"); 1.0 }
    lazy val e: Byte = { println("Byte"); 1 }
    lazy val f: Char = { println("Char"); '1' }
    lazy val g: Short = { println("Short"); 1 }
    lazy val h: String = { println("String"); "1" }
    lazy val i: Unit = { println("Unit"); () }

    a
    b
    c
    d
    e
    f
    g
    h
    // i // FIXME: See #5350
  }
}
