// this file must *not* be first lexicographically because of order of operations
trait T:
  self: S.type =>
  import X.m
  def f = m
