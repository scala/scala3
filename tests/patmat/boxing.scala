class C {
  def matchUnboxed(i: Integer) = i match {
    case null => 0
    case 1    => 1
    case _    => 9
  }

  def matchBoxed(i: Int) = i match {
    case C.ZERO => 0
    case C.ONE  => 1
    case _      => 9
  }
}

object C {
  final val ZERO: Integer = 0
  final val ONE: Integer  = 1
}
