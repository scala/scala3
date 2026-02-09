
object Bug:

  def m0(f: PartialFunction[Char, Unit]): Unit = ()

  def m1(): Unit =
    m0: x =>
      "abc".filter(_ == x) match
        case _ => ()

  def m2(): Unit =
    m0: x =>
      x match
        case _ => ()

