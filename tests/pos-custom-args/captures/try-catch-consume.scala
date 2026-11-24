import language.experimental.separationChecking

class Ref extends caps.ExclusiveCapability

def eat(consume r: Ref^): Unit = ()

object E1:
  def unapply(e: Throwable): Option[String] = Some(e.getMessage)

object E2:
  def unapply(e: Throwable): Option[Int] = Some(0)

def test(consume r: Ref^): Unit =
  try ???
  catch
    case E1(_) => eat(r)
    case E2(_) => eat(r)
