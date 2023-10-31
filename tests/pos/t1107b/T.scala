sealed trait Top
sealed trait Sub extends Top
trait C {
  private object P extends Sub
  def bob(): Class[?] = P.getClass
  def bob2() = O.d(P)
}
