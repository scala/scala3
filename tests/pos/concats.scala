object Test {
  type Name = String
  val CommonOpNames: Set[Name]   = Set("OR", "XOR")
  val ConversionNames: Set[Name] = Set("toByte")
  val BooleanOpNames: Set[Name]  = Set("ZOR") ++ CommonOpNames
  val NumberOpNames: Set[Name]   = (
       Set("ADD")
    ++ Set("UNARY_+", "UNARY_-")
    ++ CommonOpNames
  )
}