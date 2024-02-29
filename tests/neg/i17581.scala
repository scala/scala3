import scala.reflect.Selectable.reflectiveSelectable

class Test

def foo[A <: { def bar: Any }](ob: A) = ob.bar

@main def main =
  val test = new Test
  foo(test) // error // was NoSuchMethodException
