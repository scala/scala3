
class Container[Rs <: Tuple]:
  def guard(a: Tuple.Tail[Rs]): Unit = ()

val _ = Container[(1,2,3)].guard((6, 8): Tuple.Tail[(5, 6, 8)]) // error
