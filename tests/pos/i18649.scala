object Test:
  // always inferred Nothing for `x`
  def contextFunctionWildcardExplicit: ? ?=> String = x ?=> "foo"

  // used to infer TYPEBOUNDS for the type of the argument
  def contextFunctionWildcardInserted: ? ?=> String = "foo"
end Test
