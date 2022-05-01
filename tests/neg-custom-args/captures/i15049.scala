class Session:
  def request = "Response"
class Foo:
  private val session: {*} Session = new Session
  def withSession[T](f: ({*} Session) => T): T = f(session)

def Test =
  val f = new Foo
  f.withSession(s => s).request // error
  f.withSession[{*} Session](t => t) // error
