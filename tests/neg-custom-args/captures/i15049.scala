class Session:
  def request = "Response"
class Foo:
  private val session: Session^{cap} = new Session
  def withSession[sealed T](f: Session^ => T): T = f(session)

def Test: Unit =
  val f = new Foo
  f.withSession(s => s).request // error
  f.withSession[Session^](t => t) // error
