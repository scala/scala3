import annotation.switch
object Test extends App {

  val x = 3
  final val Y = 3

  val x1 = x match {
    case 0 => 0
    case 1 => 1
    case 2 => 2
    case Y => 3
  }

  val x2 = (x: @switch) match {
    case 0 => 0
    case 1 | 2 => 2
    case Y => 3
    case _ => 4
  }

  val x3 = (x: @switch) match {
    case '0' if x > 0 => 0
    case '1' => 1
    case '2' => 2
    case '3' => 3
    case x => 4
  }

  assert(x1 == 3)
  assert(x2 == 3)
  assert(x3 == 4)
}
