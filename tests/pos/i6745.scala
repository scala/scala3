trait Foo { self =>
  type M
  def apply(prog: (h: this.type) => h.M): M = prog(this)
}