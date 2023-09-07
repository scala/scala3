abstract class LIST[+T]:
  def map[U](f: T => U): LIST[U] = ???

class C
type Cap = C^

def test(d: Cap) =
  val zsc: LIST[Cap ->{d} Unit] = ???
  val a4 = zsc.map[Cap ->{d} Unit]((x: Cap ->{d} Unit) => x)
  val a5 = zsc.map[Cap ->{d} Unit](identity[Cap ->{d} Unit])
  val a6 = zsc.map(identity[Cap ->{d} Unit])
  val a7 = zsc.map(identity)
