import language.experimental.captureChecking

class Box[T](items: Seq[T^]):
  def getOne: T^{items*} = ???

object Box:
  def getOne[T](items: Seq[T^]): T^{items*} =
    val bx = Box(items)
    bx.getOne