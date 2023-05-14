package test

object Test:
  val cov1: List[Boolean]          = Macro.cov(List(true))
  val inv1: Set[Boolean]           = Macro.inv(Set(true))
  def cov2[X](a: List[X]): List[X] = Macro.cov(a)
  def inv2[X](a: Set[X]): Set[X]   = Macro.inv(a) // doesn't compile; Set[Nothing] is inferred
