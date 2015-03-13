object test123 {
  type A = { def a: Int }
  def f(a: A): A = a
}

object structural2 {
  type A = { def a: Int }

  type B = {
    def b: Int
  }

  type AB = A & B

  def f(ab: AB): AB = ab

  f(new {
    def a = 43
    def b = 42
  })
}