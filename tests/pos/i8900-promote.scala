class Inv[A <: Singleton](x: A)
object Inv {
  def empty[A <: Singleton]: Inv[A] = new Inv(???)
}

class Inv2[A](x: A)
object Inv2 {
  def empty[A]: Inv2[A] = new Inv2(???)
}

object Test {
  def inv(cond: Boolean) = // used to leak: Inv[x.type]
    if (cond)
      val x: Int = 1
      new Inv(x)
    else
      Inv.empty
}
