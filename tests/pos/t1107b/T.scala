sealed trait Top
sealed trait Sub extends Top
trait C {
  private object P extends Sub
  def bob(): Class[_] = P.getClass
  def bob2() = O.d(P)
}
