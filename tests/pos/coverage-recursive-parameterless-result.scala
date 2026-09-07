import language.experimental.captureChecking

class SWE96RecursiveAccessorRepro { self: SWE96RecursiveAccessorRepro^ =>
  def inner: this.type = this
  def outer: inner.type = inner

  def build(p: Any^): SWE96RecursiveAccessorWithFilter^{this, p} =
    new SWE96RecursiveAccessorWithFilter(outer, p)
}

class SWE96RecursiveAccessorWithFilter(
  val source: SWE96RecursiveAccessorRepro^,
  val predicate: Any^
)
