object unboundWildcard {

  val wildcardVal: _ = 0  // error: unbound wildcard type

  val annotated: _ @unchecked = 0  // error: unbound wildcard type

  def wildcardArg(x: _): Int = 0  // error: unbound wildcard type

  def wildcardResult(x: Int): _ = 0  // error: unbound wildcard type

  val singletonTuple: (((((((_))))))) = ???  // error: unbound wildcard type

  val wildcardBoundedTypeArgL: List[_ <: _] = List(0)  // error: unbound wildcard type
  val wildcardBoundedTypeArgU: List[_ >: _] = List(0)  // error: unbound wildcard type

  def wildcardBoundedTypeParamL[T <: _](x: T): T = x  // error: unbound wildcard type
  def wildcardBoundedTypeParamU[T >: _](x: T): T = x  // error: unbound wildcard type

  val _1403: (_ <: Any) = 1  // error: unbound wildcard type
}
