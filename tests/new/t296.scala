object Bug {
  def foo (l: => String, l1: => String) : String = 12 match {
    case 12 => l1
    case _ => l}
}
