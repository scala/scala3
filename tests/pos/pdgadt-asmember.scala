// Taken from https://github.com/lampepfl/dotty/pull/14754#issuecomment-1157427912.
trait T[X]
case object Foo extends T[Unit]

trait AsMember {
  type L
  val tl: T[L]
}

def testMember(am: AsMember): Unit =
  am.tl match {
    case Foo => println(summon[am.L =:= Unit])
    case _ => ()
  }
