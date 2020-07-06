object PostConditions1 {

  import PostConditions.{ensure, res, Box}

  val v = List(1, 2, 4).sum.ensure(Box(10) => res == 10) // error: not a legal formal parameter
  println(v)
}

object PostConditions {

  class Box[T](val t: T)

  def res[T](using b: Box[T]): T = b.t

  extension [T](e: T) def ensure (cond: Box[T] ?=> Boolean): T = {
    if (cond(using Box(e))) e
    else throw new AssertionError("condition not fulfilled")
  }
}