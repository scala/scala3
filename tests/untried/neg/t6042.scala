trait LazyExp[+Self <: LazyExp[Self]] { this: Self =>
  type OpSemExp <: LazyExp[OpSemExp] with Self
  type Val
}

object Test {
  def foo[AA <: LazyExp[_]](a: AA): a.OpSemExp#Val = ??? // a.OpSemExp is volatile, because of `with Self`
}
