import scala.annotation.tailrec

// putting @tailrec through the paces
object Winners {
  @tailrec
  def facsucc(n: Int, acc: Int): Int =
    if (n == 0) acc
    else facsucc(n - 1, n * acc)

  @tailrec def loopsucc1(x: Int): Int = loopsucc1(x - 1)
  @tailrec def loopsucc2[T](x: Int): Int = loopsucc2[T](x - 1)

  def ding(): Unit = {
    object dong {
      @tailrec def loopsucc3(x: Int): Int = loopsucc3(x)
    }
    ()
  }

  def inner(q: Int) = {
    @tailrec
    def loopsucc4(x: Int): Int = loopsucc4(x + 1)

    loopsucc4(q)
  }

  object innerBob {
    @tailrec def loopsucc5(x: Int): Int = loopsucc5(x)
  }
}

class Winners {
  @tailrec private def succ1(x: Int): Int = succ1(x)
  @tailrec final def succ2(x: Int): Int = succ2(x)
  @tailrec final def succ3[T](in: List[T], acc: List[T]): List[T] = in match {
    case Nil      => Nil
    case x :: xs  => succ3(xs, x :: acc)
  }

  @tailrec final def succ4[T](x: Int): Int = succ4(x - 1)

  class Tom[T] {
    @tailrec final def succ5[U](other: Tom[U], x: Int): Int = other.succ5[U](other, x - 1)
    @tailrec final def succ6(x: Int): Int = (new Tom[Int]).succ6(x - 1)
  }
}

object Failures {
  @tailrec
  def facfail(n: Int): Int =
    if (n == 0) 1
    else n * facfail(n - 1) // error: not in tail pos
}

class Failures {
  @tailrec def fail1(x: Int): Int = fail1(x) // error: not private, not final

  // a typical between-chair-and-keyboard error
  @tailrec final def fail2[T](xs: List[T]): List[T] = xs match {
    case Nil      => Nil
    case x :: xs  => x :: fail2[T](xs) // error: not in tail pos
  }
}
