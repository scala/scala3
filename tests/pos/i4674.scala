object i4674 {

  val x: String = "test"

  x.foreach {
    case 's' => println("s")
    case c: Char => println(c)
  }
}

//New warning:
//The highlighted type test will always succeed since the scrutinee type (class Char) is the same as the tested type