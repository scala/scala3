// this file must be first lexicographically because of order of operations
trait T:
  self: S.type =>
  import X.m
  def f = m // BCodeBodyBuilder desugars m to this.T.X.m, previously failed "missing outer accessor in <root>"
            // previously failed "missing outer accessor in <root>"
            // In other compilation order, BCodeSkelBuilder has set S.X to be JavaStatic,
            // so prefix is elidable.

// class vs trait doesn't matter. Selection X.m does not demonstrate the issue.
class C:
  self: D.type =>
  def c = Y.n
