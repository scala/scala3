import scala.annotation.experimental

@experimental def x = 2

@experimental class A {
  def f = x // ok because A is experimental
}

@experimental class B {
  def f = x // ok because A is experimental
}

@experimental object C {
  def f = x // ok because A is experimental
}

@experimental class D {
  def f = {
    object B {
      x // ok because A is experimental
    }
  }
}

@experimental class E {
  def f = {
    def g = {
      x // ok because A is experimental
    }
  }
}

class F {
  def f = {
    def g = {
      x // error
    }
  }
}
