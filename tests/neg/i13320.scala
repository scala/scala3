object Foo:
  case object Boo

var x: Foo.Booo = Foo.Booo // error // error

object Main:
  def main(args: Array[String]) =
    type t = Option[Foo.Boo] // error
