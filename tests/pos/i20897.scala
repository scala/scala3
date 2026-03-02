object Test:
  type Disj[A, B] =
    A match
      case B => true
      case _ => false

  def f(a: Disj[1 | Nothing, 2 | Nothing]): Unit = ()

  val t = f(false)
end Test
