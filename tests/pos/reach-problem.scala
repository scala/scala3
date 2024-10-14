import language.experimental.captureChecking

class Box[T](items: Seq[T^]):
  def getOne: T^{items*} = ???

object Box:
  def getOne[T](items: Seq[T^]): T^{items*} =
    val bx = Box(items)
    bx.getOne
/*
  def head[T](items: Seq[T^]): Unit =
    val is = items
    val x = is.head
    ()

  def head2[X^, T](items: Seq[T^{X^}]): T^{X^} =
    items.head

  def head3[T](items: Seq[T^]): Unit =
    head2[caps.CapSet^{items*}, T](items)
*/