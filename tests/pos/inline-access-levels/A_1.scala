package test

object A {

  private var x: Int = 0

  rewrite def actOnX(f: Int => Int) = {
    x = f(x)
  }
}
