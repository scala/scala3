object unboundWildcard {

  val wildcardVal: ? = 0  // error: unbound wildcard type

  val annotated: ? @unchecked = 0  // error: unbound wildcard type

  def wildcardArg(x: ?): Int = 0  // error: unbound wildcard type

  def wildcardResult(x: Int): ? = 0  // error: unbound wildcard type

  val singletonTuple: (((((((?))))))) = ???  // error: unbound wildcard type

  val wildcardBoundedTypeArgL: List[? <: ?] = List(0)  // error: unbound wildcard type
  val wildcardBoundedTypeArgU: List[? >: ?] = List(0)  // error: unbound wildcard type

  def wildcardBoundedTypeParamL[T <: ?](x: T): T = x  // error: unbound wildcard type
  def wildcardBoundedTypeParamU[T >: ?](x: T): T = x  // error: unbound wildcard type

  val _1403: (_ <: Any) = 1  // error: unbound wildcard type
}
