class AppliedPIso[A, B]()
case class User(age: Int)

object Test:
  extension [From, To](from: From)
    def focus(): AppliedPIso[From, From] = ???
    transparent inline def focus(inline lambda: (From => To)): Any = ???


  val u = User(1)
  val ap: AppliedPIso[User, User] = u.focus(_.age) // error
