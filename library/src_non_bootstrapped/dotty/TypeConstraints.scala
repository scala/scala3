package dotty

object TypeConstraints {
  // Fallback on non phantom type constraints
  type <::<[-From, +To] = Predef.<:<[From, To]
  type =::=[From, To] = Predef.=:=[From, To]

  implicit def cast_<::<[From, To](x: From)(implicit ev: From <::< To): To = x.asInstanceOf[To]

  implicit def cast_=::=[From, To](x: From)(implicit ev: From =::= To): To = x.asInstanceOf[To]
  implicit def inv_cast_=::=[From, To](x: To)(implicit ev: From =::= To): From = x.asInstanceOf[From]
}
