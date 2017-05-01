class Inv[A](x: A)
object Inv {
  def empty[A]: Inv[A] = new Inv(???)
}

class Inv2[A](x: A)
object Inv2 {
  def empty[A]: Inv2[A] = new Inv2(???)
}

object Test {
  def inv(cond: Boolean) =
    if (cond)
      new Inv(1)
    else
      Inv.empty

  val x: Inv[Int] = inv(true)

  def inv2(cond: Boolean): Inv[Int] | Inv2[Int] =
    if (cond) {
      if (cond)
        new Inv(1)
      else
        Inv.empty
    } else {
      if (cond)
        new Inv2(1)
      else
        Inv2.empty
    }

  val y: Inv[Int] | Inv2[Int] = inv2(true)
}
