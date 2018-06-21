object i4674 {

  val x: String = "test"

  x.foreach {
    case 's' => println("s")
    case c: Char => println(c)
  }
}
