class Lift {
  def apply(f: F0) // error
  class F0
  object F0 { implicit def f2f0(String): F0 = ??? } // error
  (new Lift)("") // error after switch to approximating asSeenFrom
}
