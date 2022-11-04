def test() = {
  func(_ => Box(Seq.empty[String]) )
}

def func[R0](to0: Unit => R0): Unit = ???

trait JsonFormat[T]
object JsonFormat{
  implicit def immSeqFormat: JsonFormat[Seq[String]]  = ???

  implicit def iterableFormat: JsonFormat[Iterable[String]]   = ???
}

case class Box[A1: JsonFormat](elem: A1)