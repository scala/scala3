import annotation.tailrec

class Test  {
  def cond: Boolean = ???

  @tailrec final def tailCall1(x: Int): Boolean =
    if (x < 0) tailCall1(0)
    else tailCall1(x - 1) || cond // error

  @tailrec final def tailCall2(x: Int): Boolean =
    if (x < 0) tailCall2(0)
    else tailCall2(x - 1) && cond // error

  @tailrec final def tailCall3(x: Int): Boolean =
    if (x < 0) tailCall3(0)
    else cond || tailCall3(x - 1)

  @tailrec final def tailCall4(x: Int): Boolean =
    if (x < 0) tailCall4(0)
    else cond && tailCall4(x - 1)
}
