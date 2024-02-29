trait Dependency {
  trait More
}

trait MyFunc {
  def apply(a: Int, b: String)(using dep: Dependency, more: dep.More): String
}

case class Context(f: MyFunc)

def test = Context(f = (_, _) => ???) // error // error
