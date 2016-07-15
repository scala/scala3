object Test {

  def f[C[+X]] = ()

  class D[X] {}

  f[D] // error

  def g[E[-Y]] = f[E] // error

}
