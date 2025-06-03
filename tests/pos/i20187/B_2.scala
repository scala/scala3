import scala.deriving.Mirror

def Test =
  summon[Mirror.Of[F]] // ok
  summon[Mirror.Of[G]] // was crash
  summon[Mirror.Of[H]] // was crash
  summon[Mirror.Of[I]] // was crash
