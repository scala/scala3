package test

object A {

  private var x: Int = 0

  transparent def actOnX(f: Int => Int) = {
    x = f(x)
  }
}
