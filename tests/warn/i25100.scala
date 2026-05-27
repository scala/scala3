//> using options -language:strictEquality -Werror -Wunused:all

case class Foo(value: String)
case object Bar

object Import {
  given CanEqual[Foo | Bar.type, Foo | Bar.type] = CanEqual.derived
}

@main
def main(): Unit = {
  import Import.given
  val i: Foo | Bar.type = Foo("Foo")

  i match {
    case i: Foo => println("i is a Foo")
    case Bar => println("i is a Long")
  }
}

class StatusCode(val code: Int) extends AnyVal
val Ok = StatusCode(200)

def amethod =
  given CanEqual[StatusCode, StatusCode] = CanEqual.derived
  Ok match
    case Ok =>
      "ok"
    case _ =>
      "other"
