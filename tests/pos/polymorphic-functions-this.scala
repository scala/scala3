trait Foo:
  type X
  def x: X
  val f: [T <: this.X] => (T, this.X) => (T, this.X) =
    [T <: this.X] => (x: T, y: this.X) => (x, y)
  f(x, x)
