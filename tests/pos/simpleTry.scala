
class Foo {
  try 3
  catch {
    case e: Throwable => 4
  } finally println(6)
}
