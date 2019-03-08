object Main {
  val a: Int = 4
  a match {
    case 1 => println("1")
    case 1 | 2 => println("1 or 2")
  }

  a match {
    case 1 => 1
    case 0 | 0 => 0
    case 2 | 2 | 2 | 3 | 2 | 3 => 0
    case 4 | (_ @ 4) => 0
    case _ => -1
  }

  a match {
    case 1 => 1
    case 0 | 0 => 0
    case 2 | 2 | 2 | 3 | 2 | 3 => 0
    case _ => -1
  }

  a match {
    case 0 | 1 => 0
    case 1 => 1
  }
}
