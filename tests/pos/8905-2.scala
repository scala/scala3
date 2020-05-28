class Test[T1 <: Tuple, T2 <: Tuple] {
  def test6[Y <: Int, X <: Function1[Tuple.Concat[T1, T2], Unit]](x: X) = x.isInstanceOf[Function1[Int, Any]]
}
