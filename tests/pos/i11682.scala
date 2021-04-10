object Test {
  final class Tag[T]

  def foo[Z >: Int <: Int, Y >: Z <: Z, X >: Y <: Y, T]: Tag[T] => T = {
    case _ : Tag[X] => 0
  }
}

