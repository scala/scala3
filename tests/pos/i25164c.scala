import scala.language.experimental.modularity

class Box(tracked val value: Any)

def test(): Unit =
  val optBox: Option[Box("hello")] = Some(Box("hello")) // was crash
