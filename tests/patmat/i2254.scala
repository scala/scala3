object Test {
  type OrAlias = Int | Float

  def m(s: OrAlias | String) = s match {
    case _: Int => ; case _: Float => ; case _: String => ; }
}