import scala.language.experimental.modularity

abstract class Entry(tracked val origin: Entry):
  def walk(): List[Entry(origin)]

def test(e: Entry): List[String] =
  e.walk().flatMap { entry => // was crash
    List(entry.toString)
  }
