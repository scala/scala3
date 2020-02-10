trait SAMTrait: // error
  def first(): String
  def equals[T >: Boolean <: Boolean](obj: Any): T
