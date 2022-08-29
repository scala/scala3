abstract class LIST[+T]:
  def map[U](f: T => U): LIST[U] = ???

class C
type Cap = {*} C

def test(d: Cap) =
  val zsc: LIST[{d} Cap -> Unit] = ???
  val a4 = zsc.map[{d} Cap -> Unit]((x: {d} Cap -> Unit) => x)
  val a5 = zsc.map[{d} Cap -> Unit](identity[{d} Cap -> Unit])
  val a6 = zsc.map(identity[{d} Cap -> Unit])
  val a7 = zsc.map(identity)
