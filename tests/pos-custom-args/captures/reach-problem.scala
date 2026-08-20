import language.experimental.captureChecking

class Box[T, c^](items: Seq[T^{c}]):
  def getOne: T^{c} = ???

object Box:
  def getOne[T, c^](items: Seq[T^{c}]): T^{c} =
    val bx = Box(items)
    bx.getOne

  def head[T, c^](items: Seq[T^{c}]): Unit =
    val is = items
    val x = is.head
    ()

  def head2[T, c^](items: Seq[T^{c}]): T^{c} =
    items.head

  def head3[T, d^](items: Seq[T^{d}]): Unit =
    head2[T, {d}](items)
