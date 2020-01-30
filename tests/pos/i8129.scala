object Test {
  type F[N <: 0 | 1] = N
  def fl[N <: 0 | 1]: F[N] = ???
}