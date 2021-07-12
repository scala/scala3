inline def summonA[T](using x: T): x.type = x
inline def summonB[T](using inline x: T): x.type = x
inline def summonC[T](using inline x: T): T = x

trait Foo:
  def f: Int = 9

def test(using Foo) =
  summonA[Foo].f
  summonB[Foo].f
  summonC[Foo].f
  ()
