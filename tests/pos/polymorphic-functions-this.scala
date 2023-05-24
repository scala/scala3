trait Foo:
  type X
  def x: X
  val f: [T <: this.X] => (T, this.X) => (T, this.X) =
    [T <: this.X] => (x: T, y: this.X) => (x, y)
  f(x, x)

  val g: [T <: this.type] => (T, this.type) => (T, this.type) =
    [T <: this.type] => (x: T, y: this.type) => (x, y)
  g(this, this)
