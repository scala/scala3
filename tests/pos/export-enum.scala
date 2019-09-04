object Signature {

  enum MatchDegree {
    case NoMatch, ParamMatch, FullMatch
  }
  export MatchDegree._

  // Check that exported values have singeleton types
  val x: MatchDegree.NoMatch.type = NoMatch

  // Check that the following two methods are not exported.
  // Exporting them would lead to a double definition.
  def values: Array[MatchDegree] = ???
  def valueOf($name: String): MatchDegree = ???
}
