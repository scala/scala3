import scala.compiletime.summonInline

type A
given A = ???

inline def summon1: A = summonInline[A]
transparent inline def summon2: A = summonInline[A]

def test1 = summon1
def test2 = summon2
