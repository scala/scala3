trait Dependency

trait MyFunc {
  def apply(a: Int, b: String)(using Dependency): String
}

case class Context(f: MyFunc)

def test = Context(f = (_, _) => ???) // error // error
