// this file must be first lexicographically because of order of operations
trait T:
  self: S.type =>
  import X.m
  def f = m

// class vs trait doesn't matter. Selection X.m does not demonstrate the issue.
class C:
  self: D.type =>
  def c = X.m
